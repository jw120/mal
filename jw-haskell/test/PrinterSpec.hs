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

module PrinterSpec
  ( spec
  )
where

import           Data.Text                      ( Text )
import           Test.Hspec

import           Mal                            ( AST(..) )
import           Printer
import           TestHelpers                    ( i
                                                , kw
                                                , list
                                                , m
                                                , s
                                                , sym
                                                , vec
                                                )


malFormat' :: AST -> Text
malFormat' = malFormat True

spec :: Spec
spec = do
  describe "mal formatter " $ do
    it "formats a symbol" $ do
      malFormat' (sym "abc") `shouldBe` "abc"
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



