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

import qualified Data.Map                      as M

import qualified Env
import           Types                          ( AST(..)
                                                , EnvRef
                                                , Mal
                                                , Text
                                                , extractSym
                                                )

-- | Main evaluation function
eval :: EnvRef -> AST -> Mal AST

-- Special form: do
eval _ (ASTList [ASTSym "do"]) = throwError "No arguments for do special form"
eval envRef (ASTList (ASTSym "do" : args)) = do
  args' <- mapM (eval envRef) args
  return $ last args'

-- Special form: if
eval envRef (ASTList [ASTSym "if", condArg, thenArg, elseArg]) = do
  condVal <- eval envRef condArg
  case condVal of
    ASTNil   -> eval envRef elseArg
    ASTFalse -> eval envRef elseArg
    _        -> eval envRef thenArg
eval envRef (ASTList [ASTSym "if", condArg, thenArg]) =
  eval envRef (ASTList [ASTSym "if", condArg, thenArg, ASTNil])
eval _ (ASTList (ASTSym "if" : _)) = throwError "Bad syntax in if special form"

-- Special form: def!
eval envRef (ASTList [ASTSym "def!", ASTSym sym, val]) = do
  val' <- eval envRef val
  Env.set envRef sym val'
  return val'
eval _ (ASTList (ASTSym "def!" : _)) =
  throwError "Bad syntax in def! special form"

-- Special form: let*
eval envRef (ASTList [ASTSym "let*", ASTList bindings, val]) = do
  subEnvRef <- Env.new envRef
  addBindings subEnvRef bindings
  eval subEnvRef val
 where
  addBindings :: EnvRef -> [AST] -> Mal ()
  addBindings e (ASTSym s : v : rest) = do
    v' <- eval e v
    Env.set e s v'
    addBindings e rest
  addBindings _ [] = return ()
  addBindings _ _  = throwError "Unexpected value in bindings in let*"
eval envRef (ASTList [ASTSym "let*", ASTVector bindings, val]) =
  eval envRef (ASTList [ASTSym "let*", ASTList bindings, val])
eval _ (ASTList (ASTSym "let*" : _)) =
  throwError "Bad syntax in let* special form"

-- Special form: fn*
eval envRef (ASTList [ASTSym "fn*", ASTList binds, body]) = do
  binds' <- mapM extractSym binds
  return . ASTFunc $ closure binds'
 where
  closure :: [Text] -> [AST] -> Mal AST
  closure bindNames args = do
    subEnvRef <- Env.new envRef
    addBindingLists subEnvRef bindNames args
    eval subEnvRef body
  -- Add bindings to the environment as two lists (a b) (2 3), catches clojure-style "&"
  addBindingLists :: EnvRef -> [Text] -> [AST] -> Mal ()
  addBindingLists e ["&", symVariadic] valVariadic = do
    valVariadic' <- mapM (eval e) valVariadic
    Env.set e symVariadic (ASTList valVariadic')
    return ()
  addBindingLists e (sym : symRest) (val : valRest) = do
    val' <- eval e val
    Env.set e sym val'
    addBindingLists e symRest valRest
  addBindingLists _ [] [] = return ()
  addBindingLists _ _  _  = throwError "Unexpected value in add Bindings"
eval envRef (ASTList [ASTSym "fn*", ASTVector binds, body]) =
  eval envRef (ASTList [ASTSym "fn*", ASTList binds, body])
eval _ (ASTList (ASTSym "fn*" : _)) =
  throwError "Bad syntax in fn* special form"

-- Special form: quote
eval _ (ASTList [ASTSym "quote", val]) = return val
eval _ (ASTList (ASTSym "quote" : _)) =
  throwError "Bad syntax in quote special form"

-- Special form: quasi-quote
eval envRef (ASTList [ASTSym "quasiquote", ast]) = eval envRef $ quasiQuote ast
 where
  quasiQuote :: AST -> AST
  quasiQuote (ASTList [ASTSym "unquote", x]) = x
  quasiQuote (ASTList (ASTList [ASTSym "splice-unquote", x] : ys)) =
    concatQ x ys
  quasiQuote (ASTList (ASTVector [ASTSym "splice-unquote", x] : ys)) =
    concatQ x ys
  quasiQuote (ASTList   (x : ys)) = consQQ x ys
  quasiQuote (ASTVector (x : ys)) = consQQ x ys
  quasiQuote x                    = ASTList [ASTSym "quote", x]
  concatQ x ys = ASTList [ASTSym "concat", x, quasiQuote (ASTList ys)]
  consQQ x ys = ASTList [ASTSym "cons", quasiQuote x, quasiQuote (ASTList ys)]
eval _ (ASTList (ASTSym "quasiquote" : _)) =
  throwError "Bad syntax in quasiquote special form"

-- Evaluation for a non-empty list
eval envRef (ASTList (func : args)) = do
  func' <- eval envRef func
  case func' of
    ASTFunc f -> do
      args' <- mapM (eval envRef) args
      f args'
    _ -> throwError "Not a function"

-- Evaluation for a vector: map eval over the vector
eval envRef (ASTVector xs) = ASTVector <$> mapM (eval envRef) xs

-- Evaluation for a map: map eval over the values
eval envRef (ASTMap    m ) = do
  elems' <- mapM (eval envRef) $ M.elems m
  return . ASTMap . M.fromList $ zip (M.keys m) elems'

-- Evaluation for a symbol: replace it with its looked up value
eval envRef (ASTSym s) = Env.get envRef s

-- Evaluation for anything else: pass through unchanged
eval _      other      = return other




