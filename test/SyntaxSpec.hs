module SyntaxSpec where

import Prelude hiding (unlines)
import Test.Hspec
import Syntax
import Text.Parsec
import Data.Text
import AST


shouldSucceedWith :: (Eq a, Show a) => (Either ParseError a) -> a -> Expectation
shouldSucceedWith (Right result) expected = result `shouldBe` expected
shouldSucceedWith (Left failure) _ = fail $ "Should succeed parsing but did not, result was " ++ (show failure)

shouldFail :: (Show a) => (Either ParseError a) -> Expectation
shouldFail (Right result) = fail $ "Should fail parsing but result was " ++ (show result)
shouldFail (Left _) = return ()

parseSimple :: IParsec a -> Text -> Either ParseError a
parseSimple parser input = runP parser topLevel "testcase" input

spec :: Spec
spec = do

    describe "skipSpaces" $ do
        it "should successfully parse the empty string" $
            (parseSimple skipSpaces "") `shouldSucceedWith` ()
        it "should successfully parse a string of spaces" $
            (parseSimple skipSpaces "       ") `shouldSucceedWith` ()

    describe "nextWordOrIndented" $ do
        it "correctly parses a word on the same line" $
            let input = "a      b" :: Text
                parser = string "a" >> (nextWordOrIndented $ char 'b')
            in (parseSimple parser input) `shouldSucceedWith` 'b'
        it "correctly parses a word on the next line" $
            let input = unlines [ "a   ", "  b" ] :: Text
                parser = char 'a' >> (nextWordOrIndented $ char 'b')
            in (parseSimple parser input) `shouldSucceedWith` 'b'
        it "correctly parses a define with identifier on next line" $
            let input = unlines [ "define   ", "  b" ] :: Text
                parser = string "define" >> (nextWordOrIndented $ char 'b')
            in (parseSimple parser input) `shouldSucceedWith` 'b'

    describe "definition parsing" $ do
        it "succeeds on a single line definition" $
            let program = "define mytest of Integer as 5" :: Text
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "succeeds on a single line definition with multiple spaces" $
            let program = "define   mytest  of     Integer   as           5" :: Text
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "fails when spaces are missing" $
            let program = "define   mytest  of     Integer   as5" :: Text
            in shouldFail $ parseSimple definition program
        it "succeeds on correctly indented two line line input" $
            let program = unlines ["define", "  mytest of Integer as 5"]
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "succeeds on correctly indented three line line input" $
            let program = unlines ["define mytest", "  of Integer", "  as 5"]
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "succeeds on correctly indented four line line input" $
            let program = unlines ["define", "  mytest", "    of Integer", "    as 5" ]
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "fails on incorrectly indented two line input" $
            let program = unlines [ "define mytest", " of Integer as 5" ]
            in shouldFail (parseSimple definition program)
        it "succeeds on correctly indented stretched out definition" $
            let program = unlines ["define", "  mytest", "    of ", "      Integer", "    as", "      5" ]
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))
        it "succeeds on correctly indented four line line input with blank lines in between" $
            let program = unlines ["define", "  mytest", "", "    of Integer", "     ", "    as 5" ]
            in (parseSimple definition program) `shouldSucceedWith` (define "mytest" (Just $ Var "Integer") (Lit (IntLit 5)))

    describe "Term parsing" $ do
        it "correctly parses a String" $
            let expression = "\"foo\""
            in (parseSimple term expression) `shouldSucceedWith` (Lit (StringLit "foo"))
        it "correctly parses an Int" $
            let expression = "22"
            in (parseSimple term expression) `shouldSucceedWith` (Lit (IntLit 22))
        it "correctly parses an identifier" $
            let expression = "myIdentifier"
            in (parseSimple term expression) `shouldSucceedWith` (Var "myIdentifier")
        it "correctly parses function application" $
            let expression = "foo 123"
            in (parseSimple term expression) `shouldSucceedWith` (App (Var "foo") (Lit (IntLit 123)))
        it "correctly parses a simple lambda expression" $
            let expression = "( x -> bar x )"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (App (Var "bar") (Var "x")))
        it "correctly parses lambda inline application" $
            let expression = "( x -> bar x ) 5"
            in (parseSimple term expression) `shouldSucceedWith` (App (lambda "x" (App (Var "bar") (Var "x"))) (Lit (IntLit 5)))
        it "correctly parses lambda inline application without spaces" $
            let expression = "(x->bar x)5"
            in (parseSimple term expression) `shouldSucceedWith` (App (lambda "x" (App (Var "bar") (Var "x"))) (Lit (IntLit 5)))
        it "parses multiple successive applications left associatively" $
            let expression = "foo   bar   baz"
            in (parseSimple term expression) `shouldSucceedWith` (App (App (Var "foo") (Var "bar")) (Var "baz"))
        it "parses multiple successive applications with parentheses to the left" $
            let expression = "((foo   bar)   baz)"
            in (parseSimple term expression) `shouldSucceedWith` (App (App (Var "foo") (Var "bar")) (Var "baz"))
        it "parses multiple successive applications with parentheses to the right" $
            let expression = "(foo   (bar   baz) )"
            in (parseSimple term expression) `shouldSucceedWith` (App (Var "foo") (App (Var "bar") (Var "baz")) )
        it "correctly parses an expression with redundant parentheses" $
            let expression = "((  (( foo (( (bar) )) ))))"
            in (parseSimple term expression) `shouldSucceedWith` (App (Var "foo") (Var "bar"))
        it "fails if there are too many closing parentheses" $
            let expression = "((( bar ))))"
            in shouldFail (parseSimple (term >> eof) expression)
        it "fails if there are too many opening parentheses" $
            let expression = "((((( bar ))))"
            in shouldFail (parseSimple term expression)
        it "fails if there are too many opening parentheses" $
            let expression = "((((( bar ))))"
            in shouldFail (parseSimple term expression)
        it "fails on empty parentheses" $
            let expression = "()"
            in shouldFail (parseSimple term expression)
        it "fails on nested empty parentheses" $
            let expression = "(())"
            in shouldFail (parseSimple term expression)
        it "fails if term is stretched across multiple lines" $
            let expression = unlines [ "f   ", "  5" ]
            in shouldFail (parseSimple (term >> eof) expression)

    describe "Lambda parsing" $ do
        it "unfolds multiple lambdas given sequence of bound variables" $
            let expression = "x y z -> bar x y"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (lambda "y" (lambda "z" (App (App (Var "bar") (Var "x")) (Var "y")) )))
        it "parses multiple lambdas in sequence" $
            let expression = "x -> y -> z -> bar x y"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (lambda "y" (lambda "z" (App (App (Var "bar") (Var "x")) (Var "y")) )))
        it "parses multiple lambdas in sequence with parentheses" $
            let expression = "x -> (y ->( z -> bar x y) )"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (lambda "y" (lambda "z" (App (App (Var "bar") (Var "x")) (Var "y")) )))
        it "parses a lambda applied to a lambda" $
            let expression = "(x->x)( x->x )"
            in (parseSimple term expression) `shouldSucceedWith` (App (lambda "x" (Var "x")) (lambda "y" (Var "y")))
        it "parses a lambda that maps to a number" $
            let expression = "x->5"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (Lit $ IntLit 5))
        it "parses a lambda that maps to a string" $
            let expression = "x->\"5\""
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (Lit $ StringLit "5"))
        it "fails if part of the variable list of a lambda is not an identifier" $
            let expression = "x 5 y -> 5"
            in (parseSimple term expression) `shouldSucceedWith` (lambda "x" (Lit $ StringLit "5"))



