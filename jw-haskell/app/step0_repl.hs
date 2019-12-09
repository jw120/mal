{-|
Module      : step0_repl
Description : Main program module for step 0
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Simple main program to pass step 0 tests - minimal repl

-}

module Main
  ( main
  )
where

import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

main :: IO ()
main = do
  x <- readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      putStrLn line
      main
