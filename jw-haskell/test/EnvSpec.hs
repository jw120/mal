{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

{-|
Module      : EnvSpec
Description : Hspec tests for Env module
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Hspec tests for Env module

-}

module EnvSpec
  ( spec
  )
where

import           Test.Hspec

import           Env
import           Types                          ( Env )
import           TestHelpers                    ( i
                                                , isErrorMatching
                                                , s
                                                )

parent :: Env
parent = set "A" (i 23) $ set "B" (i 12) empty

child :: Env
child = set "C" (i 7) $ push empty parent

grandChild :: Env
grandChild = set "D" (s "QQ") $ push empty child

spec :: Spec
spec = do
  describe "get in parent" $ do
    it "works with valid key" $ do
      ("A" `get` parent) `shouldBe` Right (i 23)
      ("B" `get` parent) `shouldBe` Right (i 12)
    it "works with not found key" $ do
      ("C" `get` parent) `shouldSatisfy` isErrorMatching "not found"
      ("D" `get` parent) `shouldSatisfy` isErrorMatching "not found"
      ("E" `get` parent) `shouldSatisfy` isErrorMatching "not found"
  describe "get in child" $ do
    it "works with valid key" $ do
      "A" `get` child `shouldBe` Right (i 23)
      "B" `get` child `shouldBe` Right (i 12)
      "C" `get` child `shouldBe` Right (i 7)
    it "works with not found key" $ do
      "D" `get` child `shouldSatisfy` isErrorMatching "not found"
      "E" `get` child `shouldSatisfy` isErrorMatching "not found"
  describe "get in grandchild" $ do
    it "works with valid key" $ do
      "A" `get` grandChild `shouldBe` Right (i 23)
      "B" `get` grandChild `shouldBe` Right (i 12)
      "C" `get` grandChild `shouldBe` Right (i 7)
      "D" `get` grandChild `shouldBe` Right (s "QQ")
    it "works with not found key" $ do
      "E" `get` grandChild `shouldSatisfy` isErrorMatching "not found"
