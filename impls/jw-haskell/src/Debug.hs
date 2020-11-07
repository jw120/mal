{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Debug
Description : Debug output
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Provide functions that give debug output to stdout from the Mal monad

-}

module Debug
  ( printInfo
  )
where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.IORef
import qualified Data.Text.IO                  as TIO

import           Printer                        ( malFormat
                                                , malPrint
                                                )
import           Types                          ( AST(..)
                                                , noMeta
                                                , EnvRef
                                                , Env(..)
                                                , Mal
                                                , throwString
                                                , Text
                                                , Config(..)
                                                )

-- | If debugging is enabled, print the given message, value and environment
printInfo :: Text -> EnvRef -> AST -> Mal ()
printInfo msg envRef ast = do
  debug <- asks configDebug
  when debug $ do
    liftIO (TIO.putStr (msg <> ": "))
    ast' <- liftIO $ malFormat True ast
    liftIO . TIO.putStr $ ast' <> " "
    env <- liftIO $ readIORef envRef
    case envToMap env of
      []      -> throwString "Unexpected empty environment"
      envList -> mapM_ malPrint envList


-- Helper function to covert an Environment to a list of ASTMaps
envToMap :: Env -> [AST]
envToMap Env { envTable = table, envOuterRef = _ } = [ASTMap noMeta table]
