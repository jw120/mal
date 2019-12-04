{- HLINT ignore "Redundant do" -}

module EvalSimpleSpec (spec) where

import Test.Hspec

import EvalSimple
import Reader (AST(..))

spec :: Spec
spec = do
  describe "simple mal evaluator" $ do
    it "works" $ do
      "NYI" `shouldBe` "Finished"
