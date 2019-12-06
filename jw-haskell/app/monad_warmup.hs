module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.State

-- State monad only

testState :: State Int String
testState = do
  x <- get
  modify (* 7)
  y <- get
  put 15
  z <- get
  return $ "State monad, expecting (7,14,10): " ++ show (x, y, z)

f :: Int -> Either String Int
f 0 = throwError "Zero!"
f n = return n

-- Exception monad only

testExcept1 :: Except String String
testExcept1 = do
  _ <- throwError "Err!"
  return "Will not be used"

testExcept2 :: Except String String
testExcept2 = do
  x <- liftEither $ f 3
  return $ show x

testExcept3 :: Except String String
testExcept3 = do
  x <- liftEither $ f 0
  return $ show x

-- State and Exception monad combined in a simple way

type Combined a = ExceptT String (State Int) a

testCombined1 :: Combined String
testCombined1 = do
  x <- get
  modify (* 3)
  y <- get
  put 10
  z <- get
  return $ "Combined monad: " ++ show (x, y, z)

testCombined2 :: Combined String
testCombined2 = do
  x <- get
  modify (* 4)
  y <- get
  put 12
  _ <- throwError "Combined err! 2"
  z <- get
  return $ "Combined monad: " ++ show (x, y, z)

testCombined3 :: Combined String
testCombined3 = do
  x <- get
  modify (* 5)
  y <- liftEither $ f 0
  put 13
  _ <- throwError "Combined err! 3"
  z <- get
  return $ "Combined monad: " ++ show (x, y, z)

main :: IO ()
main = do
  putStrLn $ evalState testState 7
  print $ runExcept testExcept1
  print $ runExcept testExcept2
  print $ runExcept testExcept3
  print $ evalState (runExceptT testCombined1) 10
  print $ evalState (runExceptT testCombined2) 10
  print $ evalState (runExceptT testCombined3) 10


