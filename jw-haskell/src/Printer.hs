module Printer
  ( malPrint
  )
where

import           Data.Text                      ( Text )

import           Reader                         ( AST )

malPrint :: Either Text AST -> IO ()
malPrint = print
