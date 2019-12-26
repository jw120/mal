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

import           Data.IORef
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Types                          ( AST(..)
                                                , LVType(..)
                                                , magicKeywordPrefix
                                                , Mal
                                                )

-- | Print our AST or error message to IO
malPrint :: AST -> Mal ()
malPrint ast = do
  t <- liftIO $ malFormat True ast
  liftIO $ TIO.putStrLn t

-- | Convert our AST or error message to text
malFormat :: Bool -> AST -> IO Text
malFormat readable ast = do
  ast' <- fmt ast
  return $ addSpaces ast'
 where
  fmt :: AST -> IO [Text]
  fmt (ASTSym t) = return [t]
  fmt (ASTInt i) = return [T.pack (show i)]
  fmt (ASTStr t) =
    return $ case (T.stripPrefix magicKeywordPrefix t, readable) of
      (Just kw, _    ) -> [":" <> kw]
      (Nothing, True ) -> ["\"" <> makeReadable t <> "\""]
      (Nothing, False) -> [t]
  fmt ASTNil        = return ["nil"]
  fmt ASTTrue       = return ["true"]
  fmt ASTFalse      = return ["false"]
  fmt ASTFM{}       = return ["#<function>"]
  fmt (ASTAtom ref) = do
    val  <- readIORef ref
    val' <- fmt val
    return $ ["(", "atom"] ++ val' ++ [")"]
  fmt (ASTLV _ LVList xs) = do
    xs' <- mapM fmt xs
    return $ ["("] ++ concat xs' ++ [")"]
  fmt (ASTLV _ LVVector xs) = do
    xs' <- mapM fmt xs
    return $ ["["] ++ concat xs' ++ ["]"]
  fmt (ASTMap _ m) = do
    xs' <- mapM fmt . unwrapPairs $ M.toList m
    return $ ["{"] ++ concat xs' ++ ["}"]
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
