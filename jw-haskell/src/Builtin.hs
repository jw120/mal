{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Builtin
Description : Builtin functions for our intpreter
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Haskell implementations of builtins

-}

module Builtin
  ( addBuiltIns
  -- We export the four arithmetic functions for use in EvalSimple
  , addition
  , subtraction
  , multiplication
  , division
  )
where

import qualified Env                            ( set )
import           Mal                            ( AST(..)
                                                , Env
                                                , Text
                                                )

addBuiltIns :: Env -> Env
addBuiltIns =
  Env.set "+" (ASTBuiltin addition)
    . Env.set "-" (ASTBuiltin subtraction)
    . Env.set "*" (ASTBuiltin multiplication)
    . Env.set "/" (ASTBuiltin division)
    . Env.set "list" (ASTBuiltin list)
    . Env.set "count" (ASTBuiltin count)
    . Env.set "empty?" (ASTBuiltin emptyTest)
    . Env.set "list?" (ASTBuiltin listTest)
    . Env.set "=" (ASTBuiltin equality)
    . Env.set ">" (ASTBuiltin (binaryIntOp (>)))
    . Env.set ">=" (ASTBuiltin (binaryIntOp (>=)))
    . Env.set "<" (ASTBuiltin (binaryIntOp (<)))
    . Env.set "<=" (ASTBuiltin (binaryIntOp (<=)))

addition :: [AST] -> Either Text AST
addition asts = do
  xs <- mapM extractIntLit asts
  return $ ASTInt (sum xs)

subtraction :: [AST] -> Either Text AST
subtraction []          = Left "Arugment error: at least one argument required"
subtraction [ASTInt i ] = Right (ASTInt (-i))
subtraction [_        ] = Left "Type error: integer expected"
subtraction (hd : rest) = do
  hd'   <- extractIntLit hd
  rest' <- mapM extractIntLit rest
  return . ASTInt $ hd' - sum rest'

multiplication :: [AST] -> Either Text AST
multiplication asts = do
  xs <- mapM extractIntLit asts
  return $ ASTInt (product xs)

division :: [AST] -> Either Text AST
division []          = Left "Arugment error: at least one argument required"
division [ASTInt _ ] = Left "Arugment error: at least one argument required"
division [_        ] = Left "Type error: integer expected"
division (hd : rest) = do
  hd'    <- extractIntLit hd
  rest'  <- mapM extractIntLit rest
  result <- hd' `safeDiv` product rest'
  return $ ASTInt result

safeDiv :: Int -> Int -> Either Text Int
safeDiv _ 0 = Left "Division by zero error"
safeDiv i j = Right $ i `div` j

list :: [AST] -> Either Text AST
list = Right . ASTList

listTest :: [AST] -> Either Text AST
listTest (ASTList _ : _) = Right ASTTrue
listTest _               = Right ASTFalse

count :: [AST] -> Either Text AST
count (ASTList xs : _) = Right (ASTInt (length xs))
count _                = Right (ASTInt 0)

emptyTest :: [AST] -> Either Text AST
emptyTest (ASTList [] : _) = Right ASTTrue
emptyTest _ = Right ASTFalse

equality :: [AST] -> Either Text AST
equality [] = Left "Expecting two arguments for equality testing"
equality [_] = Left "Expecting two arguments for equality testing"
equality (a : b : _)
  | a == b = Right ASTTrue
  | otherwise = Right ASTFalse

binaryIntOp :: (Int -> Int -> Bool) -> [AST] -> Either Text AST
binaryIntOp _ [] = Left "Expecting two arguments"
binaryIntOp _ [_] = Left "Expecting two arguments"
binaryIntOp op ((ASTInt a) : (ASTInt b) : _)
  | a `op` b = Right ASTTrue
  | otherwise = Right ASTFalse
binaryIntOp _ _ = Right ASTFalse

-- Helper function to convert a list of IntLits to Int (or return Nothing if a type error)
extractIntLit :: AST -> Either Text Int
extractIntLit (ASTInt i) = Right i
extractIntLit _          = Left "Type error: integer expected"
