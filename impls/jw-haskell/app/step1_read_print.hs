{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : step1_read_print
Description : Main program module for step 1
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Simple main program to pass step 0 tests - read/print only

Uses Mal monad to allow re-use of full read and print functions, does not
use its functionality

-}

module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Types                          ( Mal(..)
                                                , Text
                                                , Config(..)
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )

import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

main :: IO ()
main =
  void . runReaderT (runExceptT (unMal repl)) $ Config { configDebug = False }

-- Read-evaluate-print
rep :: Text -> Mal ()
rep src = case malRead src of
  Left  readError  -> liftIO $ TIO.putStrLn ("Read error: " <> readError)
  Right Nothing    -> return ()
  Right (Just ast) -> malPrint ast

-- repl - iterate rep repeatedly
repl :: Mal ()
repl = do
  x <- liftIO $ readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      liftIO $ addHistory line
      rep $ T.pack line
      repl

