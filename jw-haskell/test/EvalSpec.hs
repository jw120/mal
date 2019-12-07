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
import           Mal                            ( AST(..) )
import           Reader                         ( malRead )
import           TestHelpers                    ( i
                                                , isErrorMatching
                                                , kwText
                                                , list
                                                , m
                                                , s
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

  describe "step 2 eval functionality - basics" $ do
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


  describe "step 3 eval functionality - environment" $ do

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
      testSeq ["(def! a 4)", "(let* (q 9) q)"] $ i 9
      testSeq ["(def! a 4)", "(let* (q 9) a)"] $ i 4
      testSeq ["(def! a 4)", "(let* (z 2) (let* (q 9) a))"] $ i 4

    it "Testing let* with vector bindings" $ do
      test "(let* [z 9] z)" $ i 9
      test "(let* [p (+ 2 3) q (+ 2 p)] (+ p q))" $ i 12

    it "Testing vector evaluation" $ do
      test "(let* (a 5 b 6) [3 4 a [b 7] 8])"
        $ vec [i 3, i 4, i 5, vec [i 6, i 7], i 8]

  describe "step 4 eval functionality - if, fn, do" $ do

    it "Testing list functions" $ do
      test "(list)" $ ASTList []
      test "(list? (list))"    ASTTrue
      test "(empty? (list))"   ASTTrue
      test "(empty? (list 1))" ASTFalse
      test "(list 1 2 3)" $ ASTList [i 1, i 2, i 3]
      test "(count (list 1 2 3))" $ i 3
      test "(count (list))" $ i 0
      test "(count nil)" $ i 0
      test "(if (> (count (list 1 2 3)) 3) 89 78)" $ i 78
      test "(if (>= (count (list 1 2 3)) 3) 89 78)" $ i 89

    it "Testing if form" $ do
      test "(if true 7 8)" $ i 7
      test "(if false 7 8)" $ i 8
      test "(if false 7 false)" ASTFalse
      test "(if true (+ 1 7) (+ 1 8))" $ i 8
      test "(if false (+ 1 7) (+ 1 8))" $ i 9
      test "(if nil 7 8)" $ i 8
      test "(if 0 7 8)" $ i 7
      test "(if (list) 7 8)" $ i 7
      test "(if (list 1 2 3) 7 8)" $ i 7
      test "(= (list) nil)" ASTFalse

    it "Testing 1-way if form" $ do
      test "(if false (+ 1 7))" ASTNil
      test "(if nil 8)"         ASTNil
      test "(if nil 8 7)" $ i 7
      test "(if true (+ 1 7))" $ i 8

    it "Testing basic conditionals" $ do
      test "(= 2 1)"       ASTFalse
      test "(= 1 1)"       ASTTrue
      test "(= 1 2)"       ASTFalse
      test "(= 1 (+ 1 1))" ASTFalse
      test "(= 2 (+ 1 1))" ASTTrue
      test "(= nil 1)"     ASTFalse
      test "(= nil nil)"   ASTTrue
      test "(> 2 1)"       ASTTrue
      test "(> 1 1)"       ASTFalse
      test "(> 1 2)"       ASTFalse
      test "(>= 2 1)"      ASTTrue
      test "(>= 1 1)"      ASTTrue
      test "(>= 1 2)"      ASTFalse
      test "(< 2 1)"       ASTFalse
      test "(< 1 1)"       ASTFalse
      test "(< 1 2)"       ASTTrue
      test "(<= 2 1)"      ASTFalse
      test "(<= 1 1)"      ASTTrue
      test "(<= 1 2)"      ASTTrue

    it "Testing equality" $ do
      test "(= 1 1)"                   ASTTrue
      test "(= 0 0)"                   ASTTrue
      test "(= 1 0)"                   ASTFalse
      test "(= true true)"             ASTTrue
      test "(= false false)"           ASTTrue
      test "(= nil nil)"               ASTTrue
      test "(= (list) (list))"         ASTTrue
      test "(= (list 1 2) (list 1 2))" ASTTrue
      test "(= (list 1) (list))"       ASTFalse
      test "(= (list) (list 1))"       ASTFalse
      test "(= 0 (list))"              ASTFalse
      test "(= (list) 0)"              ASTFalse
      test "(= (list nil) (list))"     ASTFalse

    it "Testing builtin and user defined functions" $ do
      test "(+ 1 2)" $ i 3
      test "( (fn* (a b) (+ b a)) 3 4)" $ i 7
      test "( (fn* () 4) )" $ i 4
      test "( (fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)" $ i 8

    it "Testing closures" $ do
      test "( ( (fn* (a) (fn* (b) (+ a b))) 5) 7)" $ i 12
      testSeq
          [ "(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))"
          , "(def! plus5 (gen-plus5))"
          , "(plus5 7)"
          ]
        $ i 12
      testSeq
          [ "(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))"
          , "(def! plus7 (gen-plusX 7))"
          , "(plus7 8)"
          ]
        $ i 15
      testSeq ["(do (def! a 6) 7 (+ a 8))", "a"] $ i 6

    it "Testing special form case-sensitivity" $ do
      testSeq ["(def! DO (fn* (a) 7))", "(DO 3)"] $ i 7

    it "Testing recursive sumdown function" $ do
      testSeq
          [ "(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))"
          , "(sumdown 1)"
          ]
        $ i 1
      testSeq
          [ "(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))"
          , "(sumdown 2)"
          ]
        $ i 3
      testSeq
          [ "(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))"
          , "(sumdown 6)"
          ]
        $ i 21

    it "Testing recursive fibonacci function" $ do
      testSeq
          [ "(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"
          , "(fib 1)"
          ]
        $ i 1
      testSeq
          [ "(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"
          , "(fib 2)"
          ]
        $ i 2
      testSeq
          [ "(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"
          , "(fib 4)"
          ]
        $ i 5

    it "Testing recursive function in environment." $ do
      test "(let* (cst (fn* (n) (if (= n 0) nil (cst (- n 1))))) (cst 1))"
           ASTNil
      test
          "(let* (f (fn* (n) (if (= n 0) 0 (g (- n 1)))) g (fn* (n) (f n))) (f 2))"
        $ i 0

    it "Testing if on strings" $ do
      test "(if \"\" 7 8)" $ i 7

    it "Testing string equality" $ do
      test "(= \"\" \"\")"       ASTTrue
      test "(= \"abc\" \"abc\")" ASTTrue
      test "(= \"abc\" \"\")"    ASTFalse
      test "(= \"\" \"abc\")"    ASTFalse
      test "(= \"abc\" \"def\")" ASTFalse
      test "(= \"abc\" \"ABC\")" ASTFalse
      test "(= (list) \"\")"     ASTFalse
      test "(= \"\" (list))"     ASTFalse

    it "Testing variable length arguments" $ do
      test "( (fn* (& more) (count more)) 1 2 3)" $ i 3
      test "( (fn* (& more) (list? more)) 1 2 3)" ASTTrue
      test "( (fn* (& more) (count more)) 1)" $ i 1
      test "( (fn* (& more) (count more)) )" $ i 0
      test "( (fn* (& more) (list? more)) )" ASTTrue
      test "( (fn* (a & more) (count more)) 1 2 3)" $ i 2
      test "( (fn* (a & more) (count more)) 1)" $ i 0
      test "( (fn* (a & more) (list? more)) 1)" ASTTrue

    it "Testing language defined not function" $ do
      test "(not false)" ASTTrue
      test "(not nil)"   ASTTrue
      test "(not true)"  ASTFalse
      test "(not \"a\")" ASTFalse
      test "(not 0)"     ASTFalse

    it "Testing str" $ do
      test "(str)" $ s ""
      test "(str \"\")" $ s ""
      test "(str \"abc\")" $ s "abc"
      test "(str \"\\\")" $ s "\""
      test "(str 1 \"abc\" 3)" $ s "1abc3"
      test "(str \"abc  def\" \"ghi jkl\")" $ s "abc  defghi jkl"
      test "(str \"abc\\ndef\\nghi\")" $ s "abc\\ndef\\nghi"
      test "(str \"abc\\\\def\\\\ghi\")" $ s "abc\\\\def\\\\ghi"
      test "(str (list 1 2 \"abc\" \"\\\"\") \"def\")" $ s "(1 2 abc \\\")def"
      test "(str (list))" $ s "()"

    it "Testing keywords" $ do
      test "(= :abc :abc)"               ASTTrue
      test "(= :abc :def)"               ASTFalse
      test "(= :abc \":abc\")"           ASTFalse
      test "(= (list :abc) (list :abc))" ASTTrue

    it "Testing vector truthiness" $ do
      test "(if [] 7 8)" $ i 7

    it "Testing vector functions" $ do
      test "(count [1 2 3])" $ i 3
      test "(empty? [1 2 3])" ASTFalse
      test "(empty? [])"      ASTTrue
      test "(list? [4 5 6])"  ASTFalse

    it "Testing vector equality" $ do
      test "(= [] (list))"        ASTTrue
      test "(= [7 8] [7 8])"      ASTTrue
      test "(= [:abc] [:abc])"    ASTTrue
      test "(= (list 1 2) [1 2])" ASTTrue
      test "(= (list 1) [])"      ASTFalse
      test "(= [] [1])"           ASTFalse
      test "(= 0 [])"             ASTFalse
      test "(= [] 0)"             ASTFalse
      test "(= [] \"\")"          ASTFalse
      test "(= \"\" [])"          ASTFalse

    it "Testing vector parameter lists" $ do
      test "( (fn* [] 4) )" $ i 4
      test "( (fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)" $ i 8

    it "Nested vector/list equality" $ do
      test "(= [(list)] (list []))" ASTTrue
      test "(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))" ASTTrue




