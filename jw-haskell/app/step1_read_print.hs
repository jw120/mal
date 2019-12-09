{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import qualified Env
import           Types                          ( Mal(..)
                                                , Text
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )

import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

main :: IO ()
main = void . runStateT (runExceptT (unMal repl)) $ Env.emptyWithoutOuter

repl :: Mal ()
repl = do
  x <- liftIO $ readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      liftIO $ addHistory line
      rep $ T.pack line
      repl

rep :: Text -> Mal ()
rep src = case malRead src of
  Left  readError  -> liftIO $ TIO.putStrLn ("Read error: " <> readError)
  Right Nothing    -> return ()
  Right (Just ast) -> malPrint ast



