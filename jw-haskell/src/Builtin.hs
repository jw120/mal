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
  ( addition
  , subtraction
  , multiplication
  , division
  , list
  , listTest
  , count
  , emptyTest
  )
where

import           Data.Text                      ( Text )
import           Mal                            ( AST(..) )

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
emptyTest (ASTList _  : _) = Right ASTFalse
emptyTest _                = Left "Expected a list"

-- Helper function to convert a list of IntLits to Int (or return Nothing if a type error)
extractIntLit :: AST -> Either Text Int
extractIntLit (ASTInt i) = Right i
extractIntLit _          = Left "Type error: integer expected"
