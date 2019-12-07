module Main
  ( main
  )
where

import           Data.Text                      ( Text )

import           Mal                            ( AST )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )
import           Utilities                      ( readlineLoop )

malEval :: Either Text (Maybe AST) -> Either Text (Maybe AST)
malEval = id

main :: IO ()
main = readlineLoop (malPrint . malEval . malRead)
