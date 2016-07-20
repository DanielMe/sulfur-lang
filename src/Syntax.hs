
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


nextWordOrIndented :: IParsec a -> IParsec a
nextWordOrIndented nextParser = try (skipSpaces >> nextParser)
                            <|> try (skipSpaces >> newline >> skipBlankLines >> skipSpaces >> indented >> (withIndentation nextParser))

nextWordOrAligned :: IParsec a -> IParsec a
nextWordOrAligned nextParser =  try (skipSpaces >> nextParser)
                            <|> try (skipSpaces >> newline >> skipBlankLines >> skipSpaces >> align >> (withIndentation nextParser))


definition :: IParsec (Term String)
definition = withIndentation $ do
    reservedWord "define"
    nextWordOrIndented $ do
       name <- identifier
       (typeTerm, valueTerm) <- nextWordOrIndented $ do
         reservedWord "of"
         typeTerm <- nextWordOrIndented term
         valueTerm <- nextWordOrAligned $ (reservedWord "as") >> (nextWordOrIndented term)
         return (typeTerm, valueTerm)
       return $ define name (Just typeTerm) valueTerm


term :: IParsec (Term String)
term = termWithoutApplication `chainl1` (try parseApplication)

termWithoutApplication :: IParsec (Term String)
termWithoutApplication = (parens term)
    <|> try parseString
    <|> try parseInteger
    <|> try parseIdentifierSequence

parseIdentifierSequence :: IParsec (Term String)
parseIdentifierSequence = do
    firstIdentifier <- identifier :: IParsec String
    skipSpaces
    otherIdentifiers <- chainl (try parseSingletonIdentifier) (try parseMoreIdentifiers) [] :: IParsec [String]
    skipSpaces
    maybeLambdaAbstraction <- optionMaybe (reservedOperator "->" >> skipSpaces >> term) :: IParsec (Maybe (Term String))
    return $ case maybeLambdaAbstraction of
        (Just boundTerm) -> foldr lambda (boundTerm) (firstIdentifier:otherIdentifiers)
        (Nothing) -> foldl App (Var firstIdentifier) (map Var otherIdentifiers)
    where
        parseMoreIdentifiers = do
            skipSpaces
            notFollowedBy (char ')')
            notFollowedBy anyReserved
            return (++)
        parseSingletonIdentifier = do
            ident <- identifier
            return [ident]

parseApplication :: IParsec (Term a -> Term a -> Term a)
parseApplication = do
    skipSpaces
    notFollowedBy (char ')')
    notFollowedBy anyReserved
    return App
