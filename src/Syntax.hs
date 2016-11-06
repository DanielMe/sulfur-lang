
module Syntax where
import Prelude hiding (replicate)
import Text.Parsec.Text
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Data.Text hiding (concat, foldl1, map, foldr, foldl)
import AST
import Data.Functor.Identity
import Control.Monad
import Debug.Trace (trace)
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NonEmpty

-- Indentation sensitive Parsec monad.
type IParsec a = Parsec Text ParseState a

data ParseState = ParseState
  { indents :: Column
  } deriving (Show)

reservedWords = [
         "define"
    ,    "of"
    ,    "as"
    ]

reservedOperators = [ "->" ]

data WordType = OperatorWord | AlphanumericWord

operatorChar = oneOf "+=-*^-~!@#$%&|\\/<>.,`[]{}':;±§"

endOfWord :: WordType -> IParsec ()
endOfWord currentWord = peek next
    where
        next =  (void $ char ' ')
            <|> (void newline)
            <|> eof
            <|> (void $ char ')')
            <|> (void $ char '(')
            <|> changeOfWordType
            <|> (void $ char '"')
        changeOfWordType = case currentWord of
            OperatorWord -> (void letter) <|> (void digit)
            AlphanumericWord -> void operatorChar

anyReserved = foldl1 (<|>) ((map reservedWord reservedWords) ++ (map reservedOperator reservedOperators))

topLevel :: ParseState
topLevel = ParseState 1

reservedWord :: String -> IParsec ()
reservedWord s = do
    string s <?> ("reserved word '" ++ s ++ "'")
    endOfWord AlphanumericWord

reservedOperator :: String -> IParsec ()
reservedOperator s = do
    string s <?> ("reserved word '" ++ s ++ "'")
    endOfWord OperatorWord

peek :: IParsec a -> IParsec ()
peek f = (void . try . lookAhead) f

parseInteger :: IParsec (Term a)
parseInteger = integerLiteral <?> "an integer literal"
    where
        integerLiteral = do
            f <- parseSign
            n <- parseNat
            return $ Lit (IntLit $ f n)

parseIntPattern :: IParsec (PatternBuilder a)
parseIntPattern = integerLiteral <?> "an integer literal"
    where
        integerLiteral = do
            f <- parseSign
            n <- parseNat
            return $ literalPattern (IntLit $ f n)

parseStringPattern :: IParsec (PatternBuilder a)
parseStringPattern = stringLiteral  <?> "a string literal"
   where
       stringLiteral = do
           char '"'
           strings <- many character
           char '"'
           return $ literalPattern $ StringLit (concat strings)

parseNat :: IParsec Integer
parseNat = do
           intAsString <- many1 digit
           endOfWord AlphanumericWord
           return $ read intAsString

parseSign :: IParsec (Integer -> Integer)
parseSign = (char '-' >> return negate)
         <|> return id

parens :: IParsec a -> IParsec a
parens contents = do
    pos <- getPosition
    char '('
    skipSpaces
    result <- contents
    skipSpaces
    char ')' <?> ("closing parathesis ')' for the opening parenthesis in line " ++ (show $ sourceLine pos) ++ " column " ++ (show $ sourceColumn pos))
    return result

identifier :: IParsec String
identifier = do
    name <- many1 letter
    endOfWord AlphanumericWord
    guard (not $ name `elem` reservedWords) <?> "an identifier"
    return name

operator :: IParsec String
operator = do
    name <- many1 letter
    endOfWord OperatorWord
    guard (not $ name `elem` reservedWords) <?> "an operator"
    return name

escapedCharacter :: IParsec String
escapedCharacter = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

unescapedCharacter :: IParsec Char
unescapedCharacter = noneOf "\\\"\0\n\r\v\t\b\f"

character :: IParsec String
character = fmap return unescapedCharacter <|> escapedCharacter

parseString :: IParsec (Term a)
parseString = stringLiteral <?> "a string literal"
    where
        stringLiteral = do
            char '"'
            strings <- many character
            char '"'
            return $ Lit $ StringLit (concat strings)

skipSpaces :: IParsec ()
skipSpaces = skipMany $ char ' '

skipBlankLine :: IParsec ()
skipBlankLine = skipSpaces >> newline >> return ()

skipBlankLines :: IParsec ()
skipBlankLines = (many $ try skipBlankLine) >> return ()

atLeastOneSpace :: IParsec ()
atLeastOneSpace = (char ' ' >> skipSpaces) <?> "at least one space"

withIndentation :: IParsec a -> IParsec a
withIndentation m = do
    cur <- indents <$> getState
    pos <- sourceColumn <$> getPosition
    modifyState $ \st -> st { indents = pos }
    res <- m
    modifyState $ \st -> st { indents = cur }
    return res

ensureIndentation :: (Column -> Column -> Bool) -> IParsec ()
ensureIndentation compare = do
  column <- sourceColumn <$> getPosition
  current <- indents <$> getState
  guard (column `compare` current)

indented :: IParsec ()
indented = ensureIndentation (\ column current -> column == current + 2) <?> "an indented block"

align :: IParsec ()
align = ensureIndentation (==) <?> "a block on the same indentation level"


nextWord :: IParsec a -> IParsec a
nextWord nextParser = skipSpaces >> nextParser

nextLineIndented :: IParsec a -> IParsec a
nextLineIndented nextParser = skipSpaces >> newline >> skipBlankLines >> skipSpaces >> indented >> (withIndentation nextParser)

nextLineAligned :: IParsec a -> IParsec a
nextLineAligned nextParser = skipSpaces >> newline >> skipBlankLines >> skipSpaces >> align >> (withIndentation nextParser)

nextWordOrIndented :: IParsec a -> IParsec a
nextWordOrIndented nextParser = try (nextWord nextParser)
                            <|> try (nextLineIndented nextParser)

nextWordOrAligned :: IParsec a -> IParsec a
nextWordOrAligned nextParser =  try (nextWord nextParser)
                            <|> try (nextLineAligned nextParser)


definition :: IParsec (Term String)
definition = withIndentation $ do
    reservedWord "define"
    nextWordOrIndented $ do
       name <- identifier
       (parsedTypeTerm, parsedValueTerm) <- nextWordOrIndented $ do
         reservedWord "of"
         parsedTypeTerm <- nextWordOrIndented typeTerm
         parsedValueTerm <- nextWordOrAligned $ (reservedWord "as") >> (nextWordOrIndented term)
         return (parsedTypeTerm, parsedValueTerm)
       return $ define name (Just parsedTypeTerm) parsedValueTerm


-- | Parses a term in the sulfur language
--
-- We distinguish two main cases: A term can be a list of terms which corresponds to an
-- application if there is more than one or a simple term (literal or variable) if there is just
-- one.
--
-- The second main case is that the term is a pattern matching or a lambda expression.
-- This case is identified by the presence of a "->" symbol which follows a list of pattern expressions.
--
-- In some cases it might be only the "->" symbol that uniquely determines whether an expression
-- is an application or a lambda abstraction. For example:
--
-- @
--   a b c 5
--   a b c 5 -> a
-- @
--
-- The first expression is interpreted as "a" applied to "b" applied to "c" applied to the literal 5.do
-- The second expression is interpreted as a lambda with three input variables and one literal pattern
-- which matches the value 5. (This will then be resolved into 4 successive pattern matching expressions.)
term :: IParsec (Term String)
term = try patternMatching <|> try simpleTermOrApplication

typeTerm :: IParsec (Term String)
typeTerm = fmap Var identifier -- TODO parse an actual type expression

-- | Parse a nonempty list of terms
--
-- If the list of terms is a singleton, the result is just the term itself.
-- If this list contains more than one element, this is the recursive application of the head of the
-- list to the application constructed from the tail of the list.
-- For example:
--
-- @
--  a
--  a b c
-- @
--
-- The first line will parse to (Var "a"). The second line will be parsed as
-- (App (Var "a") (App (Var "b") (Var "c"))).
--
simpleTermOrApplication :: IParsec (Term String)
simpleTermOrApplication = simpleTerm `chainl1` (try parseApplication)

-- | Parses a simple term which is defined as a term that is not an application or a lambda abstraction.
--
-- A simple term is hence either a literal or a variable. It can also be a pair of parentheses which
-- in turn contain a rich term which can be an application or a lambda abstraction.
simpleTerm = (parens term)
             <|> try parseString
             <|> try parseInteger
             <|> try (fmap Var identifier)

patternMatching :: IParsec (Term String)
patternMatching = do
                  patterns <- (fmap pure simplePatternTerm) `chainl1` (try parseMultiplePatterns)
                  skipSpaces
                  reservedOperator "->"
                  skipSpaces
                  boundTerm <- term
                  return $ foldr matchLambda boundTerm patterns

-- | Any pattern including a constructor pattern
patternTerm :: IParsec (PatternBuilder String)
patternTerm = try parseConstructorPattern
            <|> try parseConstructorPattern
            <|> try simplePatternTerm

-- | A pattern term without constructor patterns (unless wrapped in parentheses)
simplePatternTerm :: IParsec (PatternBuilder String)
simplePatternTerm = (parens patternTerm)
            <|> try parseIntPattern
            <|> try parseStringPattern
            <|> try parseVariablePattern

parseVariablePattern :: IParsec (PatternBuilder String)
parseVariablePattern = fmap varPattern identifier

parseConstructorPattern :: IParsec (PatternBuilder String)
parseConstructorPattern = do
    constructor <- identifier
    subPatterns <- chainl (fmap pure simplePatternTerm) (try parseMultiplePatterns) []
    return $ constructorPattern constructor subPatterns

parseApplication :: IParsec (Term String -> Term String -> Term String)
parseApplication = do
    skipSpaces
    notFollowedBy (char ')')
    notFollowedBy anyReserved
    return App

parseMultiplePatterns :: IParsec ([PatternBuilder String] -> [PatternBuilder String] -> [PatternBuilder String])
parseMultiplePatterns = do
    skipSpaces
    notFollowedBy (char ')')
    notFollowedBy anyReserved
    return (++)
