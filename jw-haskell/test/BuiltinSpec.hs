{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module BuiltinSpec (spec) where

import Data.Text (Text, isInfixOf, toLower)
import Test.Hspec

import Builtin
import Reader (AST(..))

-- Helper function to shorten tests
i :: Int -> AST
i = ASTIntLit
ri :: Int -> Either Text AST
ri = Right . ASTIntLit
isErrorMatching :: Text -> Either Text AST -> Bool
isErrorMatching x (Left t) = (toLower x) `isInfixOf` (toLower t)
isErrorMatching _ (Right _) = False

spec :: Spec
spec = do
  describe "addition" $ do
    it "works with no arguments" $ do
      addition [] `shouldBe` ri 0
    it "works with one arguments" $ do
      addition [i 7] `shouldBe` ri 7
    it "works with two arguments" $ do
      addition [i 8, i 9] `shouldBe` ri 17
    it "works with many arguments" $ do
      addition [i 10, i 2, i 4, i 30] `shouldBe` ri 46
    it "fails with non-integers" $ do
      addition [i 10, ASTStringLit "Q"] `shouldSatisfy` isErrorMatching "type"
  describe "subtraction" $ do
    it "works with no arguments" $ do
      subtraction [] `shouldSatisfy` isErrorMatching "argument"
    it "works with one arguments" $ do
      subtraction [i 7] `shouldBe` ri (-7)
    it "works with two arguments" $ do
      subtraction [i 8, i 9] `shouldBe` ri (-1)
    it "works with many arguments" $ do
      subtraction [i 10, i 2, i 4, i 30] `shouldBe` ri (10-2-4-30)
  describe "multiplication" $ do
    it "works with no arguments" $ do
      multiplication [] `shouldBe` ri 1
    it "works with one arguments" $ do
      multiplication [i 7] `shouldBe` ri 7
    it "works with two arguments" $ do
      multiplication [i 8, i 9] `shouldBe` ri 72
    it "works with many arguments" $ do
      multiplication [i 10, i 2, i 4, i 30] `shouldBe` ri (10 * 2 * 4 * 30)
  describe "division" $ do
    it "works with no arguments" $ do
      division [] `shouldSatisfy` isErrorMatching "argument"
    it "works with one arguments" $ do
      division [i 6] `shouldSatisfy` isErrorMatching "argument"
    it "works with two arguments" $ do
      division [i 10, i 2] `shouldBe` ri 5
    it "works with many arguments" $ do
      division [i 120, i 2, i 4, i 3] `shouldBe` ri 5

