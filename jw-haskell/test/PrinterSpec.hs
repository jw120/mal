{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

{-|
Module      : PrinterSpec
Description : Hspec tests for Printer module
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Hspec tests for Printer module

-}

module PrinterSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Printer
import Reader (AST(..), MalBuiltin)
import TestHelpers (i, kw, list, m, s, sym, vec)

dummyFn :: MalBuiltin
dummyFn _ = Right (i 3)

malFormat' :: AST -> Text
malFormat' = malFormat . Right . Just

spec :: Spec
spec = do
  describe "mal formatter " $ do
    it "formats a symbol" $ do
      malFormat'(sym "abc") `shouldBe` "abc"
    it "formats an integer" $ do
      malFormat' (i 123) `shouldBe` "123"
    it "formats a negative integer" $ do
      malFormat' (i (-123)) `shouldBe` "-123"
    it "formats a string" $ do
      malFormat' (s "abc") `shouldBe` "\"abc\""
    it "formats a keyword" $ do
        malFormat' (kw "abc") `shouldBe` ":abc"
    it "formats a string with escape sequences" $ do
      malFormat' (s "a\\b\"c\nd") `shouldBe` "\"a\\\\b\\\"c\\nd\""
    it "formats a list" $ do
      malFormat' (list [i 1, i 2]) `shouldBe` "(1 2)"
    it "formats a vector" $ do
       malFormat' (vec [i 7, i 8]) `shouldBe` "[7 8]"
    it "formats a map" $ do
        malFormat' (m [("a", i 3)]) `shouldBe` "{\"a\" 3}"
    it "formats an empty AST" $ do
      malFormat (Right Nothing) `shouldBe` ""
    it "formats a function" $ do
      malFormat' (ASTBuiltin dummyFn) `shouldBe` "#<builtin-function>"

