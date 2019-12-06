{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

{-|
Module      : EvalSimpleSpec
Description : Hspec tests for EvalSimople module
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Hspec tests for EvalSimple module

-}

module EvalSimpleSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import EvalSimple (malEval)
import Reader (AST(..), malRead)
import TestHelpers (i, isErrorMatching, kwText, list, m, vec)

-- Local helper functions
test :: Text -> AST -> Expectation
test t u = (malEval . malRead) t `shouldBe` Right (Just u)
testError :: Text -> Text -> Expectation
testError t u = (malEval . malRead) t `shouldSatisfy` (isErrorMatching u)

spec :: Spec
spec = do
  describe "simple mal evaluator" $ do
    it "Testing evaluation of arithmetic operations" $ do
      test "(+ 1 2)" $ i 3
      test "(+ 5 (* 2 3))" $ i 11
      test "(- (+ 5 (* 2 3)) 3)" $ i 8
      test "(/ (- (+ 5 (* 2 3)) 3) 4)" $ i 2
      test "(/ (- (+ 515 (* 87 311)) 302) 27)" $ i 1010
      test "(* -3 6)" $ i (-18)
      test "(/ (- (+ 515 (* -87 311)) 296) 27)" $ i (-994)
      testError "(abc 1 2 3)" $ "symbol"
    it "Additional error cases" $ do
      testError "(/ 5)" $ "argument"
      testError "(/ 5 0)" $ "zero"
    it "Testing empty list" $ do
      test "()" $ list []
    it "Test evaluation within collection literals" $ do
      test "[1 2 (+ 1 2)]" $ vec [i 1, i 2, i 3]
      test "{\"a\" (+ 7 8)}" $ m [("a", i 15)]
      test "{:a (+ 7 10)}" $ m [(kwText "a", i 17)]
