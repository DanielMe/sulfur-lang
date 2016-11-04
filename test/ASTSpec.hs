module ASTSpec where

import Test.Hspec
import AST

spec :: Spec
spec = do

    describe "varPattern" $ do
        it "should bind a single variable in a lambda" $
            simpleLambda (varPattern "x") (Var "x") `shouldBe` simpleLambda (varPattern "y") (Var "y")

    describe "wildcardPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = simpleLambda (varPattern "x") (simpleLambda wildcardPattern (Var "x"))
                lambdaWithY = simpleLambda (varPattern "y") (simpleLambda wildcardPattern (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY

    describe "asPattern" $ do
         it "should bind whatever the wildcard pattern matches" $
             simpleLambda (asPattern "x" wildcardPattern) (Var "x") `shouldBe` simpleLambda (asPattern "y" wildcardPattern) (Var "y")

    describe "literalPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = simpleLambda (varPattern "x") (simpleLambda (literalPattern (StringLit "foo")) (Var "x"))
                lambdaWithY = simpleLambda (varPattern "y") (simpleLambda  (literalPattern (StringLit "foo")) (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY
    describe "constructorPattern" $ do
        it "should bind multiple variables in a lambda" $
            let lambdaAB = simpleLambda (constructorPattern "Const" [varPattern "a", varPattern "b", wildcardPattern]) (App (Var "a") (Var "b"))
                lambdaXY = simpleLambda (constructorPattern "Const" [varPattern "x", varPattern "y", wildcardPattern]) (App (Var "x") (Var "y"))
            in lambdaAB `shouldBe` lambdaXY
