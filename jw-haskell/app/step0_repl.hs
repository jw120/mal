module Main
  ( main
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO

import           Utilities                      ( readlineLoop )

dummyRead :: Text -> Text
dummyRead = id

dummyEval :: Text -> Text
dummyEval = id

dummyPrint :: Text -> IO ()
dummyPrint = TIO.putStrLn

main :: IO ()
main = readlineLoop (dummyPrint . dummyEval . dummyRead)
