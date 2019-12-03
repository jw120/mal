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
      malFormat (Right (ASTSymbol "abc")) `shouldBe` "abc"
    it "formats an integer" $ do
      malFormat (Right (ASTInt 123)) `shouldBe` "123"
    it "formats a negative integer" $ do
      malFormat (Right (ASTInt (-123))) `shouldBe` "-123"
    it "formats a string" $ do
      malFormat (Right (ASTString "abc")) `shouldBe` "\"abc\""
    it "formats a list" $ do
      malFormat (Right (ASTList [ASTInt 1, ASTInt 2])) `shouldBe` "(1 2)"