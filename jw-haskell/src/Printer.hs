{-# LANGUAGE OverloadedStrings #-}

module Printer
  ( malPrint
  , malFormat
  )
where

import           Data.List                      ( foldl' )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Reader                         ( AST(..) )

-- | Print our AST
malPrint :: Either Text AST -> IO ()
malPrint = TIO.putStrLn . malFormat

malFormat :: Either Text AST -> Text
malFormat (Left  msg) = "Error: " <> msg
malFormat (Right ast) = addSpaces $ go [] ast
 where
  go :: [Text] -> AST -> [Text]
  go acc (ASTSymbol t ) = acc ++ [t]
  go acc (ASTInt    i ) = acc ++ [T.pack (show i)]
  go acc (ASTString t ) = acc ++ ["\"" <> t <> "\""]
  go acc (ASTList   xs) = acc ++ ["("] ++ contents ++ [")"]
    where contents = concatMap (go []) xs

-- | Helper function to join a list of texts, adding spaces except after ( or before )
addSpaces :: [Text] -> Text
addSpaces xs = snd $ foldl' f ("(", T.empty) xs
 where
  f :: (Text, Text) -> Text -> (Text, Text)
  f (prev, acc) next | prev == "(" = (next, acc <> next)
                     | next == ")" = (next, acc <> next)
                     | otherwise   = (next, acc <> " " <> next)
