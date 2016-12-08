
module Syntax where
import Prelude hiding (replicate)
import Text.Megaparsec.Text
import Text.Megaparsec
import qualified Text.Megaparsec.Expr as Ex
import qualified Text.Megaparsec.Lexer as Lex
import Data.Text hiding (concat, foldl1, map, foldr, foldl, empty)
import AST
import Data.Functor.Identity
import Control.Monad
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec.Pos
import Control.Monad.State
import Control.Applicative (empty)

data ParseState = ParseState {
    indentationLevel :: Pos
} deriving (Show, Eq)

type IParsec = StateT ParseState (Parsec Dec Text)

indentationStep :: Pos -> Pos
indentationStep x = unsafePos $ (unPos x) + 2

skipWhiteSpace :: IParsec ()
skipWhiteSpace = Lex.space (char ' ' >> return ()) empty empty

skipWhiteSpaceAndNewlines = Lex.space (char ' ' <|> newline >> return ()) empty empty

reservedWords = [
         "define"
    ,    "of"
    ,    "as"
    ]

reservedOperators = [ "->" ]

data WordType = OperatorWord | AlphanumericWord

operatorChar :: IParsec Char
operatorChar = oneOf ("+=-*^-~!@#$%&|\\/<>.,`[]{}':;±§" :: String)

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
            OperatorWord -> void alphaNumChar
            AlphanumericWord -> void operatorChar

anyReserved = foldl1 (<|>) ((map reservedWord reservedWords) ++ (map reservedOperator reservedOperators))

topLevel :: ParseState
topLevel = ParseState $ unsafePos 1

parseTopLevel :: IParsec a -> IParsec a
parseTopLevel p = put topLevel >> p


reservedWord :: String -> IParsec ()
reservedWord s = lexeme $ do
    string s <?> ("reserved word '" ++ s ++ "'")
    endOfWord AlphanumericWord

reservedOperator :: String -> IParsec ()
reservedOperator s = do
    string s <?> ("reserved operator '" ++ s ++ "'")
    endOfWord OperatorWord

peek :: IParsec a -> IParsec ()
peek f = (void . try . lookAhead) f

lexeme :: IParsec a -> IParsec a
lexeme = Lex.lexeme skipWhiteSpace

signed :: (Num a) => IParsec a -> IParsec a
signed = Lex.signed empty

parseInteger :: IParsec (Term a)
parseInteger = lexeme $ do
    n <- signed Lex.integer
    return $ Lit (IntLit n)

parseIntPattern :: IParsec (PatternBuilder a)
parseIntPattern = lexeme $ do
    n <- signed Lex.integer
    return $ literalPattern (IntLit n)

parseStringPattern :: IParsec (PatternBuilder a)
parseStringPattern = lexeme stringLiteral <?> "a string literal"
   where
       stringLiteral = do
           char '"'
           strings <- many character
           char '"'
           return $ literalPattern $ StringLit (concat strings)

parens :: IParsec a -> IParsec a
parens contents = lexeme $ do
    pos <- getPosition
    char '('
    skipWhiteSpace
    result <- contents
    skipWhiteSpace
    char ')'
    return result

constructorIdentifier :: IParsec String
constructorIdentifier = lexeme $ do
  first <- upperChar
  name <- many alphaNumChar
  endOfWord AlphanumericWord
  guard (not $ name `elem` reservedWords) <?> "a constructor identifier (a sequence of alphanumeric letters starting with an upper case letter)"
  return (first : name)

typeIdentifier :: IParsec String
typeIdentifier = lexeme $ do
    first <- upperChar
    name <- many alphaNumChar
    endOfWord AlphanumericWord
    guard (not $ name `elem` reservedWords) <?> "a type identifier (a sequence of alphanumeric letters starting with an upper case letter)"
    return (first : name)


variableIdentifier :: IParsec String
variableIdentifier = lexeme $ do
    first <- lowerChar
    name <- many alphaNumChar
    endOfWord AlphanumericWord
    guard (not $ name `elem` reservedWords) <?> "a variable identifier that is not a reserved word, '" ++ name ++ "' is a reserved word."
    return (first : name)

operator :: IParsec String
operator = lexeme $ do
    name <- some operatorChar
    endOfWord OperatorWord
    guard (not $ name `elem` reservedOperators) <?> "an operator that is not a reserved word, '" ++ name ++ "' is a reserved operator."
    return name

escapedCharacter :: IParsec String
escapedCharacter = do
    d <- char '\\'
    c <- oneOf ("\\\"0nrvtbf" :: String)-- all the characters which can be escaped
    return [d, c]

unescapedCharacter :: IParsec Char
unescapedCharacter = noneOf ("\\\"\0\n\r\v\t\b\f" :: String)

character :: IParsec String
character = fmap return unescapedCharacter <|> escapedCharacter

parseString :: IParsec (Term a)
parseString = lexeme stringLiteral <?> "a string literal"
    where
        stringLiteral = do
            char '"'
            strings <- many character
            char '"'
            return $ Lit $ StringLit (concat strings)

withIndentation :: IParsec a -> IParsec a
withIndentation p = do
    previousIndentationLevel <- indentationLevel <$> get
    currentPosition <- Lex.indentLevel
    modify $ \ state -> state { indentationLevel = currentPosition }
    result <- p
    modify $ \ state -> state { indentationLevel = previousIndentationLevel }
    return result

indentGuard = Lex.indentGuard skipWhiteSpaceAndNewlines EQ

indented :: IParsec Pos
indented = do
    referenceLevel <- indentationLevel <$> get
    indentGuard (indentationStep referenceLevel)

align :: IParsec Pos
align = do
    referenceLevel <- indentationLevel <$> get
    indentGuard (referenceLevel)

nextLineIndented :: IParsec a -> IParsec a
nextLineIndented nextParser = skipWhiteSpaceAndNewlines >> indented >> (withIndentation nextParser)

nextLineAligned :: IParsec a -> IParsec a
nextLineAligned nextParser = skipWhiteSpaceAndNewlines >> align >> nextParser

nextWordOrIndented :: IParsec a -> IParsec a
nextWordOrIndented nextParser = try nextParser
                            <|> try (nextLineIndented nextParser)

nextWordOrAligned :: IParsec a -> IParsec a
nextWordOrAligned nextParser =  try nextParser
                            <|> try (nextLineAligned nextParser)


definition :: IParsec (Term String)
definition = withIndentation $ do
    reservedWord "define"
    nextWordOrIndented $ do
       name <- variableIdentifier
       (parsedTypeTerm, parsedValueTerm) <- nextWordOrIndented $ do
         reservedWord "of"
         parsedTypeTerm <- nextWordOrIndented typeTerm
         nextWordOrAligned $ (reservedWord "as")
         parsedValueTerm <- term <|> (nextLineIndented topLevelTerm)
         return (parsedTypeTerm, parsedValueTerm)
       return $ define name (Just parsedTypeTerm) parsedValueTerm

alignedPattern :: IParsec (Lambda String)
alignedPattern = do
    skipWhiteSpaceAndNewlines >> align
    (Match (pattern :| _ )) <- patternMatching
    return pattern



multilinePattern :: IParsec (Term String)
multilinePattern = do
    (Match (firstPattern :| _ )) <- patternMatching
    otherPatterns <- many alignedPattern
    return $ Match (firstPattern :| otherPatterns)


-- | Parses a term at the top level relative to a 'define' expression
--
-- Top level means at the beginning of a new block (as opposed to being a part of a sub
-- expression for instance).
-- Every regular term can also be a top level term.
-- However, a top level term can also be a multiline pattern matching which is not allowed
-- in a nested expression.
topLevelTerm :: IParsec (Term String)
topLevelTerm = try multilinePattern
            <|> try term

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
term =  try patternMatching
     <|> try simpleTermOrApplication
     <?> "a term (i.e. a pattern matching, an application, a variable or a literal)"

typeTerm :: IParsec (Term String)
typeTerm = fmap Var typeIdentifier -- TODO parse an actual type expression

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
simpleTermOrApplication = do
    firstTerm <- simpleTerm
    skipWhiteSpace
    otherTerms <- many (try simpleTerm)
    return $ foldl (App) firstTerm otherTerms


-- | Parses a simple term which is defined as a term that is not an application or a lambda abstraction.
--
-- A simple term is hence either a literal or a variable. It can also be a pair of parentheses which
-- in turn contain a rich term which can be an application or a lambda abstraction.
simpleTerm = (parens term)
        <|> (try parseString <?> "a string literal")
        <|> (try parseInteger <?> "an integer literal")
        <|> (try (fmap Var variableIdentifier) <?> "a variable identifier")
        <|> (try (fmap Var operator) <?> "an operator")

patternMatching :: IParsec (Term String)
patternMatching = do
                  firstPattern <- simplePatternTerm
                  otherPatterns <- many (try $ simplePatternTerm)
                  reservedOperator "->"
                  boundTerm <- term <?> "a term on the right hand side of the -> operator which is bound by the patterns"
                  return $ foldr matchLambda boundTerm (firstPattern:otherPatterns)

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
        <|> try parseSimpleConstructorPattern

parseVariablePattern :: IParsec (PatternBuilder String)
parseVariablePattern = fmap varPattern variableIdentifier

parseSimpleConstructorPattern :: IParsec (PatternBuilder String)
parseSimpleConstructorPattern = do
    constructor <- constructorIdentifier
    return $ constructorPattern constructor []


parseConstructorPattern :: IParsec (PatternBuilder String)
parseConstructorPattern = do
    constructor <- constructorIdentifier
    subPatterns <- many (try simplePatternTerm)
    return $ constructorPattern constructor subPatterns
