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

import           Control.Monad.Trans

import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Types                          ( AST(..)
                                                , magicKeywordPrefix
                                                , Mal
                                                )

-- | Print our AST or error message to IO
malPrint :: AST -> Mal ()
malPrint = liftIO . TIO.putStrLn . malFormat True

-- | Convert our AST or error message to text
malFormat :: Bool -> AST -> Text
malFormat readable ast = addSpaces $ concatMap fmt [ast]
 where
  fmt :: AST -> [Text]
  fmt (ASTSym t) = [t]
  fmt (ASTInt i) = [T.pack (show i)]
  fmt (ASTStr t) = case (T.stripPrefix magicKeywordPrefix t, readable) of
    (Just kw, _    ) -> [":" <> kw]
    (Nothing, True ) -> ["\"" <> makeReadable t <> "\""]
    (Nothing, False) -> [t]
  fmt ASTNil         = ["nil"]
  fmt ASTTrue        = ["true"]
  fmt ASTFalse       = ["false"]
  fmt (ASTFunc   _ ) = ["#<function>"]
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

-- | Convert a text to readable form
makeReadable :: Text -> Text
makeReadable = T.concatMap escape
 where
  escape :: Char -> Text
  escape '\n' = "\\n"
  escape '\\' = "\\\\"
  escape '\"' = "\\\""
  escape c    = T.singleton c
