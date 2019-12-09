module Main
  ( main
  )
where

import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

main :: IO ()
main = do
  x <- readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      putStrLn line
      main
