module Main
  ( main
  )
where

import           Control.Monad                  ( void )
import           Data.Text                      ( Text )

import           Env                            ( Env )
import           Eval                           ( malEval
                                                , malInitialEnv
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )
import           Utilities                      ( readlineLoopWithState )

main :: IO ()
main = void $ readlineLoopWithState malInitialEnv rep
 where
  rep :: Env -> Text -> IO Env
  rep env input = malPrint value >> return env'
    where (value, env') = malEval env (malRead input)
