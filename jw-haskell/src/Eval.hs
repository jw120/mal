{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eval
Description : AST evaluator
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

AST evaluator for steps 3 and beyond

-}

module Eval
  ( eval
  )
where

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Map                      as M

-- import Debug.Trace

import qualified Env                           as E
import           Mal                            ( AST(..)
                                                , Env
                                                , Mal
                                                )


-- | Main evaluation function
eval :: AST -> Mal AST

-- Special form: do
eval (ASTList [ASTSym "do"]) = throwError "No arguments for do special form"
eval (ASTList (ASTSym "do" : args)) = do
  args' <- mapM eval args
  return $ last args'

-- Special form: if
eval (ASTList [ASTSym "if", condArg, thenArg, elseArg]) = do
  condVal <- eval condArg
  case condVal of
    ASTNil   -> eval elseArg
    ASTFalse -> eval elseArg
    _        -> eval thenArg
eval (ASTList [ASTSym "if", condArg, thenArg]) =
  eval (ASTList [ASTSym "if", condArg, thenArg, ASTNil])
eval (ASTList (ASTSym "if" : _)) = throwError "Bad syntax in if special form"

-- Special form: def!
eval (ASTList [ASTSym "def!", ASTSym var, val]) = do
  val' <- eval val
  modify (E.set var val')
  return val'
eval (ASTList (ASTSym "def!" : _)) =
  throwError "Bad syntax in def! special form"

-- Special form: let*
eval (ASTList [ASTSym "let*", ASTList bindingPairs, val]) = do
  outerEnv <- get
  modify E.emptyWithOuter
  addBindingPairs bindingPairs
  val' <- eval val
  put outerEnv
  return val'
eval (ASTList [ASTSym "let*", ASTVector bindings, val]) =
  eval (ASTList [ASTSym "let*", ASTList bindings, val])
eval (ASTList (ASTSym "let*" : _)) =
  throwError "Bad syntax in let* special form"

-- Special form: fn*
eval (ASTList [ASTSym "fn*", ASTList binds, body]) = do
  env <- get
  throwIfNotAllSymbols binds
  return . ASTFunc $ closure env binds body
eval (ASTList [ASTSym "fn*", ASTVector binds, body]) =
  eval (ASTList [ASTSym "fn*", ASTList binds, body])
eval (ASTList (ASTSym "fn*" : _)) = throwError "Bad syntax in fn* special form"

-- Evaluation for a non-empty list
eval (ASTList (func : args)) = do
--   env <- get
--  traceM ("eval list, " ++ show func ++ " : " ++ show args ++ " env: " ++ show env)
  func' <- eval func
  case func' of
    ASTFunc f -> do
      args' <- mapM eval args
      f args'
    _ -> throwError "Not a function"

-- Evaluation for a vector: map eval over the vector
eval (ASTVector xs) = ASTVector <$> mapM eval xs

-- Evaluation for a map: map eval over the values
eval (ASTMap    m ) = do
  elems' <- mapM eval $ M.elems m
  return . ASTMap . M.fromList $ zip (M.keys m) elems'

-- Evaluation for a symbol: replace it with its looked up value
eval (ASTSym s) = do
  env <- get
  liftEither $ E.get s env

-- Evaluation for anything else: pass through unchanged
eval other = return other

-- Helper function to set new environment with bindings as an alternating list (a 2 b 3)
addBindingPairs :: [AST] -> Mal ()
addBindingPairs bindings = do
  modify E.emptyWithOuter
  go bindings
 where
  go (ASTSym s : v : rest) = do
    v' <- eval v
    modify (E.set s v')
    go rest
  go [] = return ()
  go _  = throwError "Unexpected value in addBindingPairs"

-- Helper function to set new environment with bindings as two lists (a b) (2 3)
-- Catches clojure-style "&" to capture all following arguments
addBindings :: [AST] -> [AST] -> Mal ()
addBindings syms vals = do
  modify E.emptyWithOuter
  go syms vals
 where
  go [ASTSym "&", ASTSym symVariadic] valVariadic= do
    valVariadic' <- mapM eval valVariadic
    modify (E.set symVariadic (ASTList valVariadic'))
    return ()
  go (ASTSym sym : symRest) (val : valRest) = do
    val' <- eval val
    modify (E.set sym val')
    go symRest valRest
  go [] [] = return ()
  go _  _  = throwError "Unexpected value in add Bindings"

-- Helper function to throw an error if any element of the list is not an ASTSym
throwIfNotAllSymbols :: [AST] -> Mal ()
throwIfNotAllSymbols (ASTSym _ : rest) = throwIfNotAllSymbols rest
throwIfNotAllSymbols []                = return ()
throwIfNotAllSymbols _                 = throwError "Expected a symbol"


closure :: Env -> [AST] -> AST -> [AST] -> Mal AST
closure savedEnv binds body args = do
  prevEnv <- get
  modify (E.add savedEnv)
  addBindings binds args
  retVal <- eval body
  put prevEnv
  return retVal
