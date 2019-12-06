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

import           Env                            ( Env )
import           Eval                           ( malEval
                                                , malInitialEnv
                                                )
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

-- Local helper functions: Note that each test runs in a fresh environment
test :: Text -> AST -> Expectation
test t u = fst ((malEval malInitialEnv . malRead) t) `shouldBe` Right (Just u)
testError :: Text -> Text -> Expectation
testError t u =
  fst ((malEval malInitialEnv . malRead) t) `shouldSatisfy` isErrorMatching u
-- Evaluate a sequence of expression chaining the environment from one to the next
testSeq :: [Text] -> AST -> Expectation
testSeq ts u = go malInitialEnv ts `shouldBe` Right (Just u)
 where
  go :: Env -> [Text] -> Either Text (Maybe AST)
  go env [x     ] = fst . malEval env $ malRead x
  go env (x : xs) = case malEval env (malRead x) of
    (Right _  , env') -> go env' xs
    (Left  msg, _   ) -> Left msg
  go _ [] = error "No tests in testSeq"
-- Version that ignore intermediate errors
testSeq' :: [Text] -> AST -> Expectation
testSeq' ts u = go malInitialEnv ts `shouldBe` Right (Just u)
 where
  go :: Env -> [Text] -> Either Text (Maybe AST)
  go env [x     ] = fst . malEval env $ malRead x
  go env (x : xs) = go env' xs where env' = snd . malEval env $ malRead x
  go _   []       = error "No tests in testSeq'"

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
      testSeq ["(def! x 3)", "x"] $ i 3
      testSeq ["(def! x 4)", "x"] $ i 4
      testSeq ["(def! y (+ 1 7))", "y"] $ i 8
    it "Verifying symbols are case-sensitive" $ do
      testSeq ["(def! mynum 111)", "(def! MYNUM 222)", "mynum"] $ i 111
      testSeq ["(def! mynum 111)", "(def! MYNUM 222)", "MYNUM"] $ i 222
    it "Check env lookup non-fatal error" $ do
      testError "(abc 1 2 3)" "not found"
    it "Check that error aborts def!" $ do
      testSeq' ["(def! w 123)", "(def! w (abc))", "w"] $ i 123
    it "Testing let*" $ do
      test "(let* (z 9) z)" $ i 9
      testSeq ["(def! x 4)", "(let* (x 9) x)"] $ i 9
      testSeq ["(def! x 4)", "(let* (x 9) x)", "x"] $ i 4
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
      test "(let* (a 5 b 6) [3 4 a [b 7] 8])"
        $ vec [i 3, i 4, i 5, vec [i 6, i 7], i 8]



