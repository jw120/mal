{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module PrinterSpec (spec) where

import Test.Hspec

import Printer
import Reader (AST(..), magicKeywordPrefix, MalFunction)

dummyFn :: MalFunction
dummyFn _ = Right (ASTIntLit 3)

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
    it "formats a keyword" $ do
        malFormat (Right (Just (ASTStringLit (magicKeywordPrefix <> "abc")))) `shouldBe` ":abc"
    it "formats a string with escape sequences" $ do
      malFormat (Right (Just (ASTStringLit "a\\b\"c\nd"))) `shouldBe` "\"a\\\\b\\\"c\\nd\""
    it "formats a list" $ do
      malFormat (Right (Just (ASTList [ASTIntLit 1, ASTIntLit 2]))) `shouldBe` "(1 2)"
    it "formats a vector" $ do
       malFormat (Right (Just (ASTVector [ASTIntLit 7, ASTIntLit 8]))) `shouldBe` "[7 8]"
    it "formats a map" $ do
        malFormat (Right (Just (ASTMap [ASTStringLit "a", ASTIntLit 3]))) `shouldBe` "{\"a\" 3}"
    it "formats an empty AST" $ do
      malFormat (Right Nothing) `shouldBe` ""
    it "formats a function" $ do
      malFormat (Right (Just (ASTFn dummyFn))) `shouldBe` "#<function>"

