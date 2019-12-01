module Main where

import System.Console.Readline (readline, addHistory)

myRead :: String -> String
myRead = id

eval :: String -> String
eval = id

myPrint :: String -> IO ()
myPrint = putStrLn

rep :: String -> IO ()
rep = myPrint . eval . myRead

repLoop :: IO ()
repLoop = do
  x <- readline "mal> "
  case x of
     Nothing -> return ()
     Just line -> do
        addHistory line
        rep line
        repLoop

main :: IO ()
main = repLoop
