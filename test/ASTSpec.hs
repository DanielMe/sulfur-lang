module ASTSpec where

import Test.Hspec
import AST

spec :: Spec
spec = do

    describe "varPattern" $ do
        it "should bind a single variable in a lambda" $
            matchLambda (varPattern "x") (Var "x") `shouldBe` matchLambda (varPattern "y") (Var "y")

    describe "wildcardPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = matchLambda (varPattern "x") (matchLambda wildcardPattern (Var "x"))
                lambdaWithY = matchLambda (varPattern "y") (matchLambda wildcardPattern (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY

    describe "asPattern" $ do
         it "should bind whatever the wildcard pattern matches" $
             matchLambda (asPattern "x" wildcardPattern) (Var "x") `shouldBe` matchLambda (asPattern "y" wildcardPattern) (Var "y")

    describe "literalPattern" $ do
        it "should not bind anything in a lambda" $
            let lambdaWithX = matchLambda (varPattern "x") (matchLambda (literalPattern (StringLit "foo")) (Var "x"))
                lambdaWithY = matchLambda (varPattern "y") (matchLambda  (literalPattern (StringLit "foo")) (Var "y"))
            in lambdaWithX `shouldBe` lambdaWithY
    describe "constructorPattern" $ do
        it "should bind multiple variables in a lambda" $
            let lambdaAB = matchLambda (constructorPattern "Const" [varPattern "a", varPattern "b", wildcardPattern]) (App (Var "a") (Var "b"))
                lambdaXY = matchLambda (constructorPattern "Const" [varPattern "x", varPattern "y", wildcardPattern]) (App (Var "x") (Var "y"))
            in lambdaAB `shouldBe` lambdaXY
