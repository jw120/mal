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
  )
where

import           Data.Text                      ( Text )
import           Reader                         ( AST(..) )

addition :: [AST] -> Either Text AST
addition asts = do
  xs <- mapM extractIntLit asts
  return $ ASTIntLit (sum xs)

subtraction :: [AST] -> Either Text AST
subtraction [] = Left "Arugment error: at least one argument required"
subtraction [ASTIntLit i] = Right (ASTIntLit (-i))
subtraction [_          ] = Left "Type error: integer expected"
subtraction (hd : rest  ) = do
  hd'   <- extractIntLit hd
  rest' <- mapM extractIntLit rest
  return . ASTIntLit $ hd' - sum rest'

multiplication :: [AST] -> Either Text AST
multiplication asts = do
  xs <- mapM extractIntLit asts
  return $ ASTIntLit (product xs)

division :: [AST] -> Either Text AST
division []            = Left "Arugment error: at least one argument required"
division [ASTIntLit i] = Left "Arugment error: at least one argument required"
division [_          ] = Left "Type error: integer expected"
division (hd : rest  ) = do
  hd'   <- extractIntLit hd
  rest' <- mapM extractIntLit rest
  return . ASTIntLit $ hd' `div` product rest'

-- Helper function to convert a list of IntLits to Int (or return Nothing if a type error)
extractIntLit :: AST -> Either Text Int
extractIntLit (ASTIntLit i) = Right i
extractIntLit _             = Left "Type error: integer expected"
