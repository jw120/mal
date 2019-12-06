module Main
  ( main
  )
where

import           EvalSimple                     ( malEval )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )
import           Utilities                      ( readlineLoop )

main :: IO ()
main = readlineLoop (malPrint . malEval . malRead)
