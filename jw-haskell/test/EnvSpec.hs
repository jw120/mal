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

module EnvSpec (spec) where

import Test.Hspec

import Env (Env, new, set , get)
import TestHelpers (i, isErrorMatching, s)

parent :: Env
parent = new Nothing `set` ("A", i 23) `set` ("B", i 12)

child :: Env
child = new (Just parent) `set` ("C", i 7)

grandChild :: Env
grandChild = new (Just child) `set` ("D", s "QQ")

spec :: Spec
spec = do
  describe "get in parent" $ do
    it "works with valid key" $ do
      parent `get` "A" `shouldBe` Right (i 23)
      parent `get` "B" `shouldBe` Right (i 12)
    it "works with missing key" $ do
      parent `get` "C" `shouldSatisfy` isErrorMatching "missing"
      parent `get` "D" `shouldSatisfy` isErrorMatching "missing"
      parent `get` "E" `shouldSatisfy` isErrorMatching "missing"
  describe "get in child" $ do
    it "works with valid key" $ do
      child `get` "A" `shouldBe` Right (i 23)
      child `get` "B" `shouldBe` Right (i 12)
      child `get` "C" `shouldBe` Right (i 7)
    it "works with missing key" $ do
      child `get` "D" `shouldSatisfy` isErrorMatching "missing"
      child `get` "E" `shouldSatisfy` isErrorMatching "missing"
  describe "get in grandchild" $ do
    it "works with valid key" $ do
      grandChild `get` "A" `shouldBe` Right (i 23)
      grandChild `get` "B" `shouldBe` Right (i 12)
      grandChild `get` "C" `shouldBe` Right (i 7)
      grandChild `get` "D" `shouldBe` Right (s "QQ")
    it "works with missing key" $ do
      child `get` "E" `shouldSatisfy` isErrorMatching "missing"
