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

import qualified Data.Map as M
import Test.Hspec

import Printer
import Reader (AST(..), magicKeywordPrefix, MalBuiltin)

dummyFn :: MalBuiltin
dummyFn _ = Right (ASTInt 3)

spec :: Spec
spec = do
  describe "mal formatter " $ do
    it "formats a symbol" $ do
      malFormat (Right (Just (ASTSym "abc"))) `shouldBe` "abc"
    it "formats an integer" $ do
      malFormat (Right (Just (ASTInt 123))) `shouldBe` "123"
    it "formats a negative integer" $ do
      malFormat (Right (Just (ASTInt (-123)))) `shouldBe` "-123"
    it "formats a string" $ do
      malFormat (Right (Just (ASTStr "abc"))) `shouldBe` "\"abc\""
    it "formats a keyword" $ do
        malFormat (Right (Just (ASTStr (magicKeywordPrefix <> "abc")))) `shouldBe` ":abc"
    it "formats a string with escape sequences" $ do
      malFormat (Right (Just (ASTStr "a\\b\"c\nd"))) `shouldBe` "\"a\\\\b\\\"c\\nd\""
    it "formats a list" $ do
      malFormat (Right (Just (ASTList [ASTInt 1, ASTInt 2]))) `shouldBe` "(1 2)"
    it "formats a vector" $ do
       malFormat (Right (Just (ASTVector [ASTInt 7, ASTInt 8]))) `shouldBe` "[7 8]"
    it "formats a map" $ do
        malFormat (Right (Just (ASTMap (M.fromList [("a", ASTInt 3)])))) `shouldBe` "{\"a\" 3}"
    it "formats an empty AST" $ do
      malFormat (Right Nothing) `shouldBe` ""
    it "formats a function" $ do
      malFormat (Right (Just (ASTBuiltin dummyFn))) `shouldBe` "#<builtin-function>"

