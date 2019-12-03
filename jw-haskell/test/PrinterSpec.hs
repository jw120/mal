{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module PrinterSpec (spec) where

import Test.Hspec

import Printer
import Reader (AST(..))


spec :: Spec
spec = do
  describe "mal formatter " $ do
    it "formats an integer" $ do
      malFormat (Right (ASTInt 123)) `shouldBe` "123"

