module ASTSpec where

import Test.Hspec
import AST


spec :: Spec
spec = do

    describe "varPattern" $ do
        it "should bind a single variable in a lambda" $
            lambda (varPattern "x") (Var "x") `shouldBe` lambda (varPattern "y") (Var "y")

    describe "wildcardPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = lambda (varPattern "x") (lambda wildcardPattern (Var "x"))
                lambdaWithY = lambda (varPattern "y") (lambda wildcardPattern (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY

    describe "asPattern" $ do
         it "should bind whatever the wildcard pattern matches" $
             lambda (asPattern "x" wildcardPattern) (Var "x") `shouldBe` lambda (asPattern "y" wildcardPattern) (Var "y")

    describe "literalPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = lambda (varPattern "x") (lambda (literalPattern (StringLit "foo")) (Var "x"))
                lambdaWithY = lambda (varPattern "y") (lambda  (literalPattern (StringLit "foo")) (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY
    describe "constructorPattern" $ do
        it "should bind multiple variables in a lambda" $
            let lambdaAB = lambda (constructorPattern "Const" [varPattern "a", varPattern "b", wildcardPattern]) (App (Var "a") (Var "b"))
                lambdaXY = lambda (constructorPattern "Const" [varPattern "x", varPattern "y", wildcardPattern]) (App (Var "x") (Var "y"))
            in lambdaAB `shouldBe` lambdaXY
