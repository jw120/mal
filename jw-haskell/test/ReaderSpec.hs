{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant do" -}

module ReaderSpec (spec) where

import Data.Void (Void)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse, eof, ParseErrorBundle)

import Reader

-- Helper function to make sure we test each parser fully matches and avoid repeating extra input
parse' :: Parser AST -> Text -> Either (ParseErrorBundle Text Void) AST
parse' p = parse (p <* eof) ""

spec :: Spec
spec = do
  describe "ast parser" $ do
    it "parses an integer" $ do
      parse' ast "123" `shouldParse` ASTInt 123
    it "parses an integer with leading space" $ do
      parse' ast "   123" `shouldParse` ASTInt 123
    it "parses an integer with trailing space" $ do
      parse' ast "123 " `shouldParse` ASTInt 123
    it "parses a string literal" $ do
      parse' ast "\"xyz\"" `shouldParse` ASTString "xyz"
--    it "parses a symbol name" $ do
--      parse' ast "abcd" `shouldParse` ASTSymbol "abcd"
