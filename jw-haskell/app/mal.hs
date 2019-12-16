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
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Console.Readline        ( readline
                                                , addHistory
                                                )
import           System.Environment             ( getArgs )

import qualified Core
import qualified Env
import           Eval                           ( eval )
import           Types                          ( Mal(..)
                                                , EnvRef
                                                , AST(..)
                                                , Text
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )

main :: IO ()
main = void $ runExceptT (unMal malMain)

malMain :: Mal ()
malMain = do
  envRef <- Env.empty
  Env.replaceTable envRef (Core.nameSpace envRef)
  mapM_ (rep envRef True) Core.prelude
  argv <- liftIO getArgs
  case argv of
    [] -> do
      Env.set envRef "*ARGV*" $ ASTList []
      repl envRef
    (fn : rest) -> do
      Env.set envRef "*ARGV*" . ASTList $ map (ASTStr . T.pack) rest
      let cmd = T.pack ("(load-file \"" ++ fn ++ "\")")
      rep envRef False cmd

-- Read-evaluate-print
rep :: EnvRef -> Bool -> Text -> Mal ()
rep envRef quiet src = case malRead src of
  Left  readError  -> liftIO $ TIO.putStrLn ("Read error: " <> readError)
  Right Nothing    -> return ()
  Right (Just ast) -> do
    val <- eval envRef ast `catchError` (\e -> return (ASTStr ("Error: " <> e)))
    if quiet then return () else malPrint val

-- repl - iterate rep repeatedly
repl :: EnvRef -> Mal ()
repl envRef = do
  x <- liftIO $ readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      liftIO $ addHistory line
      rep envRef False $ T.pack line
      repl envRef

