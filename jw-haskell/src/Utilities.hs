{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Utilities
Description : Utility functions
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Supporting functions

-}

module Utilities
  ( readlineLoop
  , readlineLoopWithState
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

-- | Provide simple readline interface passing each line to the given read-eval-print function
readlineLoop :: (Text -> IO ()) -> IO ()
readlineLoop rep = do
  x <- readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      rep $ T.pack line
      readlineLoop rep

-- | Provide readline interface with state
readlineLoopWithState :: s -> (s -> Text -> IO s) -> IO s
readlineLoopWithState state rep = do
  x <- readline "mal> "
  case x of
    Nothing   -> return state
    Just line -> do
      addHistory line
      state' <- rep state (T.pack line)
      readlineLoopWithState state' rep

