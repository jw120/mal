{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module PrinterSpec (spec) where

import Test.Hspec

import Printer
import Reader (AST(..))


spec :: Spec
spec = do
  describe "mal formatter " $ do
    it "formats a symbol" $ do
      malFormat (Right (Just (ASTSymbol "abc"))) `shouldBe` "abc"
    it "formats an integer" $ do
      malFormat (Right (Just (ASTIntLit 123))) `shouldBe` "123"
    it "formats a negative integer" $ do
      malFormat (Right (Just (ASTIntLit (-123)))) `shouldBe` "-123"
    it "formats a string" $ do
      malFormat (Right (Just (ASTStringLit "abc"))) `shouldBe` "\"abc\""
    it "formats a string with escape sequences" $ do
      malFormat (Right (Just (ASTStringLit "a\\b\"c\nd"))) `shouldBe` "\"a\\\\b\\\"c\\nd\""
    it "formats a list" $ do
      malFormat (Right (Just (ASTList [ASTIntLit 1, ASTIntLit 2]))) `shouldBe` "(1 2)"
    it "formats an empty AST" $ do
      malFormat (Right Nothing) `shouldBe` ""

