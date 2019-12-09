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

import qualified Env
import           Types                          ( AST(..)
                                                , Env
                                                , Mal
                                                , Text
                                                , extractSym
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
  modify (Env.set var val')
  return val'
eval (ASTList (ASTSym "def!" : _)) =
  throwError "Bad syntax in def! special form"

-- Special form: let*
eval (ASTList [ASTSym "let*", ASTList bindings, val]) = do
  oldEnv <- get
  addBindings bindings
  val' <- eval val
  put oldEnv
  return val'
 where
  addBindings :: [AST] -> Mal ()
  addBindings (ASTSym s : v : rest) = do
    v' <- eval v
    modify (Env.set s v')
    addBindings rest
  addBindings [] = return ()
  addBindings _  = throwError "Unexpected value in bindings in let*"
eval (ASTList [ASTSym "let*", ASTVector bindings, val]) =
  eval (ASTList [ASTSym "let*", ASTList bindings, val])
eval (ASTList (ASTSym "let*" : _)) =
  throwError "Bad syntax in let* special form"

-- Special form: fn*
eval (ASTList [ASTSym "fn*", ASTList binds, body]) = do
  env    <- get
  binds' <- mapM extractSym binds
  return . ASTFunc $ closure env binds'
 where
  closure :: Env -> [Text] -> [AST] -> Mal AST
  closure savedEnv binds args = do
    oldEnv <- get
    modify (Env.push savedEnv)
    addBindingLists binds args
    retVal <- eval body
    put oldEnv
    return retVal
eval (ASTList [ASTSym "fn*", ASTVector binds, body]) =
  eval (ASTList [ASTSym "fn*", ASTList binds, body])
eval (ASTList (ASTSym "fn*" : _)) = throwError "Bad syntax in fn* special form"

-- Evaluation for a non-empty list
eval (ASTList (func : args)) = do
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
  liftEither $ Env.get s env

-- Evaluation for anything else: pass through unchanged
eval other = return other

-- Helper function to add bindings to the environment as two lists (a b) (2 3)
-- Catches clojure-style "&" to capture all following arguments
addBindingLists :: [Text] -> [AST] -> Mal ()
addBindingLists ["&", symVariadic] valVariadic = do
  valVariadic' <- mapM eval valVariadic
  modify (Env.set symVariadic (ASTList valVariadic'))
  return ()
addBindingLists (sym : symRest) (val : valRest) = do
  val' <- eval val
  modify (Env.set sym val')
  addBindingLists symRest valRest
addBindingLists [] [] = return ()
addBindingLists _  _  = throwError "Unexpected value in add Bindings"


