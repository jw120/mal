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
  ( malEval
  , malInitialEnv
  )
where

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Map                      as M
import           Data.Text                      ( Text )

import Builtin (addBuiltIns)
import qualified Env                           as E
import           Mal                            ( AST(..)
                                                , Env
                                                )



malInitialEnv :: Env
malInitialEnv = addBuiltIns E.emptyWithoutOuter

-- | Top-level evaluator
--
-- Intercepts parsing errors (Left) or a missing (Nothing) and hands over to eval
malEval :: Env -> Either Text (Maybe AST) -> (Either Text (Maybe AST), Env)
malEval env (Left  err       ) = (Left err, env)
malEval env (Right Nothing   ) = (Right Nothing, env)
malEval env (Right (Just ast)) = case runState (runExceptT (eval ast)) env of
  (Left  err, env') -> (Left err, env')
  (Right x  , env') -> (Right (Just x), env')

-- We use a combined state (for the environment) and error monad
type Eval a = ExceptT Text (State Env) a

-- | Main evaluation function
eval :: AST -> Eval AST

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
  return $ ASTClosure env binds body
eval (ASTList (ASTSym "fn*" : _)) = throwError "Bad syntax in fn* special form"

-- Evaluation for a non-empty list
eval (ASTList (func : args)) = do
  func' <- eval func
  args' <- mapM eval args
  case func' of
    ASTBuiltin b              -> liftEither $ b args'
    ASTClosure env binds body -> do
      put env
      addBindings binds args
      eval body
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

-- Helper function to add bindings as an alternating list (a 2 b 3)
addBindingPairs :: [AST] -> Eval ()
addBindingPairs (ASTSym s : v : rest) = do
  v' <- eval v
  modify (E.set s v')
  addBindingPairs rest
addBindingPairs [] = return ()
addBindingPairs _  = throwError "Unexpected value in addBindingPairs"

-- Helper function to add bindings as two lists (a b) (2 3)
addBindings :: [AST] -> [AST] -> Eval ()
addBindings (ASTSym sym : symRest) (val : valRest) = do
  val' <- eval val
  modify (E.set sym val')
  addBindings symRest valRest
addBindings [] [] = return ()
addBindings _  _  = throwError "Unexpected value in add Bindings"

-- Helper function to throw an error if any element of the list is not an ASTSym
throwIfNotAllSymbols :: [AST] -> Eval ()
throwIfNotAllSymbols (ASTSym _ : rest) = throwIfNotAllSymbols rest
throwIfNotAllSymbols []                = return ()
throwIfNotAllSymbols _                 = throwError "Expected a symbol"
