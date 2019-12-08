module Main
  ( main
  )
where

import           Eval                           ( malEval )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )
import           Utilities                      ( readlineLoop )

main :: IO ()
main = readlineLoop (malPrint . malEval . malRead)
