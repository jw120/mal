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
  go acc (ASTList xs)             = acc ++ ["("] ++ concatMap (go []) xs ++ [")"]
  go acc (ASTVector xs)           = acc ++ ["["] ++ concatMap (go []) xs ++ ["]"]
  go acc (ASTMap xs)              = acc ++ ["{"] ++ concatMap (go []) xs ++ ["}"]
  escape :: Char -> Text
  escape '\n' = "\\n"
  escape '\\' = "\\\\"
  escape '\"' = "\\\""
  escape c = T.singleton c

-- | Helper function to join a list of texts with spaces
--
-- Space added between each text except not following a ( or [ and not before a ) or ]
addSpaces :: [Text] -> Text
addSpaces xs = snd $ foldl' f (True, T.empty) xs
 where
  f :: (Bool, Text) -> Text -> (Bool, Text)
  f (suppressSpace, acc) next
    | suppressSpace || isCloser next = (isOpener next, acc <> next)
    | otherwise = (isOpener next, acc <> " " <> next)
  isOpener :: Text -> Bool
  isOpener t = t == "(" || t == "[" || t == "{"
  isCloser :: Text -> Bool
  isCloser t = t == ")" || t == "]" || t == "}"
