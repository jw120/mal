module Main
  ( main
  )
where

import           Data.Text                      ( Text )


import           Printer                        ( malPrint )
import           Reader                         ( malRead
                                                , AST
                                                )
import           Utilities                      ( readlineLoop )

malEval :: Either Text (Maybe AST) -> Either Text (Maybe AST)
malEval = id

main :: IO ()
main = readlineLoop (malPrint . malEval . malRead)
