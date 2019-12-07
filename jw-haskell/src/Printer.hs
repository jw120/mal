{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Printer
Description : Convert AST to text
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Converts AST to a text representation and provides a function to print to IO

-}

module Printer
  ( malPrint
  , malFormat
  )
where

import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Mal                            ( AST(..)
                                                , magicKeywordPrefix
                                                )

-- | Print our AST or error message to IO
malPrint :: Either Text (Maybe AST) -> IO ()
malPrint = TIO.putStrLn . malFormat

-- | Convert our AST or error message to text
malFormat :: Either Text (Maybe AST) -> Text
malFormat (Left  msg       ) = "Error: " <> msg
malFormat (Right Nothing   ) = ""
malFormat (Right (Just ast)) = addSpaces $ concatMap fmt [ast]
 where
  fmt :: AST -> [Text]
  fmt (ASTSym t)     = [t]
  fmt (ASTInt i)     = [T.pack (show i)]
  fmt (ASTStr t)     = [showStringLit t]
  fmt ASTNil         = ["nil"]
  fmt ASTTrue        = ["true"]
  fmt ASTFalse       = ["false"]
  fmt (ASTBuiltin _) = ["#<builtin-function>"]
  fmt ASTClosure{}   = ["#<closure>"]
  fmt (ASTList   xs) = ["("] ++ concatMap fmt xs ++ [")"]
  fmt (ASTVector xs) = ["["] ++ concatMap fmt xs ++ ["]"]
  fmt (ASTMap m) = ["{"] ++ concatMap fmt (unwrapPairs (M.toList m)) ++ ["}"]
   where
    unwrapPairs :: [(Text, AST)] -> [AST]
    unwrapPairs ((a, b) : rest) = ASTStr a : b : unwrapPairs rest
    unwrapPairs []              = []

-- | Helper function to join a list of texts with spaces
--
-- Space added between each text except not following a ([{} and not before )]}
addSpaces :: [Text] -> Text
addSpaces xs = snd $ foldl' f (True, T.empty) xs
 where
  f :: (Bool, Text) -> Text -> (Bool, Text)
  f (suppressSpace, acc) next
    | suppressSpace || isCloser next = (isOpener next, acc <> next)
    | otherwise                      = (isOpener next, acc <> " " <> next)
  isOpener :: Text -> Bool
  isOpener = (`elem` ["(", "[", "{"])
  isCloser :: Text -> Bool
  isCloser = (`elem` [")", "]", "}"])

-- | Helper function to escape a stringlit and de-magic keywords
--
showStringLit :: Text -> Text
showStringLit t = case T.stripPrefix magicKeywordPrefix t of
  Just kw -> ":" <> kw
  Nothing -> "\"" <> T.concatMap escape t <> "\""
 where
  escape :: Char -> Text
  escape '\n' = "\\n"
  escape '\\' = "\\\\"
  escape '\"' = "\\\""
  escape c    = T.singleton c
