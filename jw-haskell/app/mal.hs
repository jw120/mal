{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : mal
Description : Main program module for mal interpreter
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Main module and drivers of repl.

-}

module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

import qualified Core
import qualified Env
import           Eval                           ( eval )
import           Types                          ( Mal(..)
                                                , AST(..)
                                                , Text
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )

main :: IO ()
main = void . runStateT (runExceptT (unMal malMain)) $ Env.new Core.nameSpace

malMain :: Mal ()
malMain = do
  mapM_ (rep True) Core.prelude
  repl

-- Read-evaluate-print
rep :: Bool -> Text -> Mal ()
rep quiet src = case malRead src of
  Left  readError  -> liftIO $ TIO.putStrLn ("Read error: " <> readError)
  Right Nothing    -> return ()
  Right (Just ast) -> do
    val <- eval ast `catchError` (\e -> return (ASTStr ("Error: " <> e)))
    if quiet then return () else malPrint val

-- repl - iterate rep repeatedly
repl :: Mal ()
repl = do
  x <- liftIO $ readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      liftIO $ addHistory line
      rep False $ T.pack line
      repl

