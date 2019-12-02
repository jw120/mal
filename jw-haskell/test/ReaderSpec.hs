{- HLINT ignore: {name: Redundant do} -}

module ReaderSpec (spec) where

import Test.Hspec

import Reader

spec :: Spec
spec = do
  describe "addTwo" $ do
    it "adds two" $ do
      addTwo 7 `shouldBe` 9
