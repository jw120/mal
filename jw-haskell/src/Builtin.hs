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
import           Reader                         ( AST(..))

addition :: [AST] -> Either Text AST
addition asts = do
  xs <- mapM extractIntLit asts
  return $ ASTIntLit (sum xs)

subtraction :: [AST] -> Either Text AST
subtraction = undefined

multiplication :: [AST] -> Either Text AST
multiplication = undefined

division :: [AST] -> Either Text AST
division = undefined


-- Helper function to convert a list of IntLits to Int (or return Nothing if a type error)
extractIntLit :: AST -> Either Text Int
extractIntLit (ASTIntLit i) = Right i
extractIntLit _ = Left "Type error: integer expected"
