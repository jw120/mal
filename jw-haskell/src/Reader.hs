{-# LANGUAGE OverloadedStrings #-}

module Reader
  ( malRead
  , AST
  , addTwo
  )
where

import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as ML

data AST
  = ASTSymbol Text
  | ASTInt Int
  | ASTList [AST]
  deriving (Show)

-- | type for our parsers (void for custom errors, text for the input type)
type Parser = M.Parsec Void Text

-- | Reader function to convert input into an AST (or to provide a parsing error)
malRead :: Text -> Either Text AST
malRead = first formatError . runParser
 where
  formatError = T.pack . M.errorBundlePretty
  runParser   = M.runParser ast "source"

-- | top level-parser for our AST
ast :: Parser AST
ast = undefined

sc :: Parser ()
sc = ML.space MC.space1 (ML.skipLineComment ";") M.empty

addTwo :: Int -> Int
addTwo x = x + 2
