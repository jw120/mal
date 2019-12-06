{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

{-|
Module      : EvalSpec
Description : Hspec tests for Eval module
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Hspec tests for Eval module

-}

module EvalSpec
  ( spec
  )
where

import           Data.Text                      ( Text )
import           Test.Hspec

import           Eval                           ( malEval )
import           Reader                         ( AST(..)
                                                , malRead
                                                )
import           TestHelpers                    ( i
                                                , isErrorMatching
                                                , kwText
                                                , list
                                                , m
                                                , vec
                                                )

-- Local helper functions
test :: Text -> AST -> Expectation
test t u = (malEval . malRead) t `shouldBe` Right (Just u)
testError :: Text -> Text -> Expectation
testError t u = (malEval . malRead) t `shouldSatisfy` isErrorMatching u
testSeq :: [Text] -> AST -> Expectation
testSeq = undefined

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
      testError "(abc 1 2 3)" "symbol"
    it "Additional error cases" $ do
      testError "(/ 5)"   "argument"
      testError "(/ 5 0)" "zero"
    it "Testing empty list" $ do
      test "()" $ list []
    it "Test evaluation within collection literals" $ do
      test "[1 2 (+ 1 2)]" $ vec [i 1, i 2, i 3]
      test "{\"a\" (+ 7 8)}" $ m [("a", i 15)]
      test "{:a (+ 7 10)}" $ m [(kwText "a", i 17)]
  describe "environment functionality" $ do
    it "Testing def!" $ do
      test "(def! x 3)" $ i 3
      test "x" $ i 3
      test "(def! x 4)" $ i 4
      test "x" $ i 4
      test "(def! y (+ 1 7))" $ i 8
      test "y" $ i 8
    it "Verifying symbols are case-sensitive" $ do
      test "(def! mynum 111)" $ i 111
      test "(def! MYNUM 222)" $ i 222
      test "mynum" $ i 111
      test "MYNUM" $ i 222
    it "Check env lookup non-fatal error" $ do
      testError "(abc 1 2 3)" "not found"
    it "Check that error aborts def!" $ do
      testSeq ["(def! w 123)", "(def! w (abc))", "w"] $ i 123
    it "Testing let*" $ do
      test "(let* (z 9) z)" $ i 9
      test "(let* (x 9) x)" $ i 9
      test "x" $ i 4
      test "(let* (z (+ 2 3)) (+ 1 z))" $ i 6
      test "(let* (p (+ 2 3) q (+ 2 p)) (+ p q))" $ i 12
      testSeq ["(def! y (let* (z 7) z))", "y"] $ i 7
    it "Testing outer environment" $ do
      test "(def! a 4)" $ i 4
      test "(let* (q 9) q)" $ i 9
      test "(let* (q 9) a)" $ i 4
      test "(let* (z 2) (let* (q 9) a))" $ i 4
    it "Testing let* with vector bindings" $ do
      test "(let* [z 9] z)" $ i 9
      test "(let* [p (+ 2 3) q (+ 2 p)] (+ p q))" $ i 12
    it "Testing vector evaluation" $ do
      test "(let* (a 5 b 6) [3 4 a [b 7] 8])" $ vec [i 3, i 4, i 5, vec [i 6, i 7], i 8]



