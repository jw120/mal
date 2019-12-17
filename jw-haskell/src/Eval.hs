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
eval _      (ASTList []      ) = return $ ASTList []
eval envRef (ASTList listArgs) = do
  expansion <- macroExpand envRef (ASTList listArgs)
  case expansion of
    ASTList expansionListArgs -> apply envRef expansionListArgs
    otherAst                  -> evalAst envRef otherAst
eval envRef ast = evalAst envRef ast

-- Helper function for eval for non-lists
evalAst :: EnvRef -> AST -> Mal AST
evalAst envRef (ASTVector xs) = ASTVector <$> mapM (eval envRef) xs
evalAst envRef (ASTMap    m ) = do
  elems' <- mapM (eval envRef) $ M.elems m
  return . ASTMap . M.fromList $ zip (M.keys m) elems'
evalAst envRef (ASTSym s) = Env.get envRef s
evalAst _      other      = return other

-- | Helper function apply to handle lists (mainly special forms)
apply :: EnvRef -> [AST] -> Mal AST

-- Special form: def!
apply envRef [ASTSym "def!", ASTSym sym, val] = do
  val' <- eval envRef val
  Env.set envRef sym val'
  return val'
apply _ (ASTSym "def!" : _) = throwError "Bad syntax in def! special form"

-- Special form: defmacro!
apply envRef [ASTSym "defmacro!", ASTSym sym, fnBody] = do
  fn <- eval envRef fnBody
  case fn of
    ASTFunc _ f -> do
      let macro = ASTFunc True f
      Env.set envRef sym macro
      return macro
    _ -> throwError "Expected a function in defmacro!"
apply _ (ASTSym "defmacro!" : _) =
  throwError "Bad syntax in defmacro! special form"

-- Special form: do
apply _ [ASTSym "do"] = throwError "No arguments for do special form"
apply envRef (ASTSym "do" : args) = do
  args' <- mapM (eval envRef) args
  return $ last args'

-- Special form: fn*
apply envRef [ASTSym "fn*", ASTList binds, body] = do
  binds' <- mapM extractSym binds
  return . ASTFunc False $ closure binds'
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
apply envRef [ASTSym "fn*", ASTVector binds, body] =
  apply envRef [ASTSym "fn*", ASTList binds, body]
apply _ (ASTSym "fn*" : _) = throwError "Bad syntax in fn* special form"

-- Special form: if
apply envRef [ASTSym "if", condArg, thenArg, elseArg] = do
  condVal <- eval envRef condArg
  case condVal of
    ASTNil   -> eval envRef elseArg
    ASTFalse -> eval envRef elseArg
    _        -> eval envRef thenArg
apply envRef [ASTSym "if", condArg, thenArg] =
  apply envRef [ASTSym "if", condArg, thenArg, ASTNil]
apply _ (ASTSym "if" : _) = throwError "Bad syntax in if special form"

-- Special form: let*
apply envRef [ASTSym "let*", ASTList bindings, val] = do
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
apply envRef [ASTSym "let*", ASTVector bindings, val] =
  apply envRef [ASTSym "let*", ASTList bindings, val]
apply _ (ASTSym "let*" : _) = throwError "Bad syntax in let* special form"

-- Special form: macroexpand
apply envRef [ASTSym "macroexpand", ast] = macroExpand envRef ast
apply _ (ASTSym "macroexpand" : _) =
  throwError "Bad syntax in macroexpand special form"

-- Special form: quote
apply _ [ASTSym "quote", val] = return val
apply _ (ASTSym "quote" : _) = throwError "Bad syntax in quote special form"

-- Special form: quasi-quote
apply envRef [ASTSym "quasiquote", ast] = eval envRef $ quasiQuote ast
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
apply _ (ASTSym "quasiquote" : _) =
  throwError "Bad syntax in quasiquote special form"

-- Evaluation for a non-empty list
apply envRef (func : args) = do
  func' <- eval envRef func
  case func' of
    ASTFunc _ f -> do
      args' <- mapM (eval envRef) args
      f args'
    _ -> throwError "Not a function"

apply _ [] = throwError "Unexpected empty list in apply" -- should not happen


macroExpand :: EnvRef -> AST -> Mal AST
macroExpand envRef ast@(ASTList (ASTSym s : otherArgs)) = do
  val <- Env.safeGet envRef s
  case val of
    Just (ASTFunc True macroFn) -> do
      retVal <- macroFn otherArgs
      macroExpand envRef retVal
    _ -> return ast
macroExpand _ ast = return ast

-- -- Is the given ast the name of a macro defined in the given environment
-- isMacroCall :: EnvRef -> AST -> Mal Bool
-- isMacroCall envRef (ASTList (ASTSym s: _)) = do
--     val <- Env.safeGet envRef s
--     case val of
--         Just (ASTFunc True _) -> return True
--         _ -> return False
-- isMacroCall _ _ = return False

