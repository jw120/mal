{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

{-|
Module      : BuiltinSpec
Description : Hspec tests for Builtin module
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Hspec tests for Builtin module

-}

module BuiltinSpec
  ( spec
  )
where

import           Test.Hspec

import           Builtin
import           TestHelpers                    ( i
                                                , isErrorMatching
                                                , s
                                                , sym
                                                )

spec :: Spec
spec = do
  describe "addition" $ do
    it "works with no arguments" $ do
      addition [] `shouldBe` Right (i 0)
    it "works with one arguments" $ do
      addition [i 7] `shouldBe` Right (i 7)
    it "works with two arguments" $ do
      addition [i 8, i 9] `shouldBe` Right (i 17)
    it "works with many arguments" $ do
      addition [i 10, i 2, i 4, i 30] `shouldBe` Right (i 46)
    it "fails with non-integers" $ do
      addition [i 10, s "Q"] `shouldSatisfy` isErrorMatching "type"
  describe "subtraction" $ do
    it "works with no arguments" $ do
      subtraction [] `shouldSatisfy` isErrorMatching "argument"
    it "works with one arguments" $ do
      subtraction [i 7] `shouldBe` Right (i (-7))
    it "works with two arguments" $ do
      subtraction [i 8, i 9] `shouldBe` Right (i (-1))
    it "works with many arguments" $ do
      subtraction [i 10, i 2, i 4, i 30] `shouldBe` Right (i (10 - 2 - 4 - 30))
    it "fails with non-integers" $ do
      subtraction [i 10, sym "Q"] `shouldSatisfy` isErrorMatching "type"
  describe "multiplication" $ do
    it "works with no arguments" $ do
      multiplication [] `shouldBe` Right (i 1)
    it "works with one arguments" $ do
      multiplication [i 7] `shouldBe` Right (i 7)
    it "works with two arguments" $ do
      multiplication [i 8, i 9] `shouldBe` Right (i 72)
    it "works with many arguments" $ do
      multiplication [i 10, i 2, i 4, i 30]
        `shouldBe` Right (i (10 * 2 * 4 * 30))
    it "fails with non-integers" $ do
      multiplication [sym "Q", i 22] `shouldSatisfy` isErrorMatching "type"
  describe "division" $ do
    it "works with no arguments" $ do
      division [] `shouldSatisfy` isErrorMatching "argument"
    it "works with one arguments" $ do
      division [i 6] `shouldSatisfy` isErrorMatching "argument"
    it "works with two arguments" $ do
      division [i 10, i 2] `shouldBe` Right (i 5)
    it "traps divide by zero" $ do
      division [i 11, i 0] `shouldSatisfy` isErrorMatching "zero"
    it "works with many arguments" $ do
      division [i 120, i 2, i 4, i 3] `shouldBe` Right (i 5)
    it "fails with non-integers" $ do
      division [s "Q", i 22] `shouldSatisfy` isErrorMatching "type"

