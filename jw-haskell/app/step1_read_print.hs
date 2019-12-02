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

malEval :: Either Text AST -> Either Text AST
malEval = id

main :: IO ()
main = readlineLoop (malPrint . malEval . malRead)
