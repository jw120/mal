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
