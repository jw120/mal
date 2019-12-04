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

import           Reader                         ( AST(..)
                                                , MalSpecialLit(..)
                                                )

-- | Print our AST
malPrint :: Either Text (Maybe AST) -> IO ()
malPrint = TIO.putStrLn . malFormat

malFormat :: Either Text (Maybe AST) -> Text
malFormat (Left  msg) = "Error: " <> msg
malFormat (Right Nothing) = ""
malFormat (Right (Just ast)) = addSpaces $ go [] ast
 where
  go :: [Text] -> AST -> [Text]
  go acc (ASTSymbol     t       ) = acc ++ [t]
  go acc (ASTIntLit     i       ) = acc ++ [T.pack (show i)]
  go acc (ASTStringLit  t       ) = acc ++ ["\"" <> T.concatMap escape t <> "\""]
  go acc (ASTSpecialLit MalNil  ) = acc ++ ["nil"]
  go acc (ASTSpecialLit MalTrue ) = acc ++ ["true"]
  go acc (ASTSpecialLit MalFalse) = acc ++ ["false"]
  go acc (ASTList xs)             = acc ++ ["("] ++ contents ++ [")"]
    where contents = concatMap (go []) xs
  escape :: Char -> Text
  escape '\n' = "\\n"
  escape '\\' = "\\\\"
  escape '\"' = "\\\""
  escape c = T.singleton c

-- | Helper function to join a list of texts, adding spaces except after ( or before )
addSpaces :: [Text] -> Text
addSpaces xs = snd $ foldl' f ("(", T.empty) xs
 where
  f :: (Text, Text) -> Text -> (Text, Text)
  f (prev, acc) next | prev == "(" = (next, acc <> next)
                     | next == ")" = (next, acc <> next)
                     | otherwise   = (next, acc <> " " <> next)
