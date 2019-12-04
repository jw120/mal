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
