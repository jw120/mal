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
import qualified Debug
import           Types                          ( AST(..)
                                                , FMType(..)
                                                , LVType(..)
                                                , EnvRef
                                                , Mal
                                                , Text
                                                , extractSym
                                                , MalError(..)
                                                , throwString
                                                )

-- | Main evaluation function (hands off to apply for lists and evalAst for non-lists)
eval :: EnvRef -> AST -> Mal AST
eval envRef ast = do
  Debug.printInfo "eval" envRef ast
  case ast of
    ASTLV meta LVList []       -> return $ ASTLV meta LVList []
    ASTLV meta LVList listArgs -> do
      expansion <- macroExpand envRef $ ASTLV meta LVList listArgs
      case expansion of
        ASTLV _ LVList expansionListArgs -> apply envRef expansionListArgs
        otherAst                         -> evalAst envRef otherAst
    _ -> evalAst envRef ast

-- Helper function for eval for non-lists
evalAst :: EnvRef -> AST -> Mal AST
evalAst envRef ast = do
  Debug.printInfo "evalAst" envRef ast
  case ast of
    ASTLV _ LVVector xs -> ASTLV ASTNil LVVector <$> mapM (eval envRef) xs
    ASTMap _ m          -> do
      elems' <- mapM (eval envRef) $ M.elems m
      return . ASTMap ASTNil . M.fromList $ zip (M.keys m) elems'
    ASTSym s -> Env.get envRef s
    _        -> return ast

-- | Helper function apply to handle lists (mainly special forms)
apply :: EnvRef -> [AST] -> Mal AST
apply envRef astList = do
  Debug.printInfo "apply" envRef $ ASTLV ASTNil LVList astList
  case astList of

      -- Special form: def!
    [ASTSym "def!", ASTSym sym, val] -> do
      val' <- eval envRef val
      Env.set envRef sym val'
      return val'
    (ASTSym "def!" : _) -> throwString "Bad syntax in def! special form"

    -- Special form: defmacro!
    [ASTSym "defmacro!", ASTSym sym, fnBody] -> do
      fn <- eval envRef fnBody
      case fn of
        ASTFM _ FMFunction f -> do
          let macro = ASTFM ASTNil FMMacro f
          Env.set envRef sym macro
          return macro
        _ -> throwString "Expected a function in defmacro!"
    (ASTSym "defmacro!" : _) ->
      throwString "Bad syntax in defmacro! special form"

    -- Special form: do
    [ASTSym "do"] -> throwString "No arguments for do special form"
    (ASTSym "do" : args) -> last <$> mapM (eval envRef) args

    -- Special form: fn*
    [ASTSym "fn*", ASTLV _ _ binds, body] -> do
      binds' <- mapM extractSym binds
      return . ASTFM ASTNil FMFunction $ closure binds'
     where
      closure :: [Text] -> [AST] -> Mal AST
      closure bindNames args = do
        subEnvRef <- Env.new envRef
        addBindingLists subEnvRef bindNames args
        Debug.printInfo "closure" subEnvRef body
        eval subEnvRef body
      -- Add bindings to the environment as two lists (a b) (2 3), catches clojure-style "&"
      addBindingLists :: EnvRef -> [Text] -> [AST] -> Mal ()
      addBindingLists e ["&", symVariadic] valVariadic = do
        Env.set e symVariadic $ ASTLV ASTNil LVList valVariadic
        return ()
      addBindingLists e (sym : symRest) (val : valRest) = do
        Env.set e sym val
        addBindingLists e symRest valRest
      addBindingLists _ [] [] = return ()
      addBindingLists _ _  _  = throwString "Unexpected value in add Bindings"
    (ASTSym "fn*" : _) -> throwString "Bad syntax in fn* special form"

    -- Special form: if
    [ASTSym "if", condArg, thenArg, elseArg] -> do
      condVal <- eval envRef condArg
      case condVal of
        ASTNil   -> eval envRef elseArg
        ASTFalse -> eval envRef elseArg
        _        -> eval envRef thenArg
    [ASTSym "if", condArg, thenArg] ->
      apply envRef [ASTSym "if", condArg, thenArg, ASTNil]
    (ASTSym "if" : _) -> throwString "Bad syntax in if special form"

    -- Special form: let*
    [ASTSym "let*", ASTLV _ _ bindings, val] -> do
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
      addBindings _ _  = throwString "Unexpected value in bindings in let*"
    (ASTSym "let*" : _)         -> throwString "Bad syntax in let* special form"

    -- Special form: macroexpand
    [ASTSym "macroexpand", ast] -> macroExpand envRef ast
    (ASTSym "macroexpand" : _) ->
      throwString "Bad syntax in macroexpand special form"

    -- Special form: quote
    [ASTSym "quote", val]      -> return val
    (ASTSym "quote" : _)       -> throwString "Bad syntax in quote special form"

    -- Special form: quasi-quote
    [ASTSym "quasiquote", ast] -> eval envRef $ quasiQuote ast
     where
      quasiQuote :: AST -> AST
      quasiQuote (ASTLV _ LVList [ASTSym "unquote", x]) = x
      quasiQuote (ASTLV _ LVList (ASTLV _ _ [ASTSym "splice-unquote", x] : ys))
        = ASTLV ASTNil
                LVList
                [ASTSym "concat", x, quasiQuote (ASTLV ASTNil LVList ys)]
      quasiQuote (ASTLV _ _ (x : ys)) = ASTLV
        ASTNil
        LVList
        [ASTSym "cons", quasiQuote x, quasiQuote (ASTLV ASTNil LVList ys)]
      quasiQuote x = ASTLV ASTNil LVList [ASTSym "quote", x]
    (ASTSym "quasiquote" : _) ->
      throwString "Bad syntax in quasiquote special form"

    -- Special form: try*/catch*
    [ASTSym "try*", ast, ASTLV _ LVList [ASTSym "catch*", ASTSym exceptionVar, catchVal]]
      -> eval envRef ast `catchError` handler
     where
      handler :: MalError -> Mal AST
      handler (MalError e) = do
        subEnvRef <- Env.new envRef
        Env.set subEnvRef exceptionVar e
        eval subEnvRef catchVal
    [ASTSym "try*", ast]     -> eval envRef ast -- try* without a catch just does eval
    (ASTSym "try*"   : _   ) -> throwString "Bad try* syntax"
    (ASTSym "catch*" : _   ) -> throwString "Bad catch* syntax"

    -- If not a special form, then apply for a non-empty list
    (func            : args) -> do
      func' <- eval envRef func
      case func' of
        ASTFM _ _ f -> do
          args' <- mapM (eval envRef) args
          f args'
        _ -> throwString "Not a function"

    -- Empty list should not be passed into apply, but catch just in case
    [] -> throwString "Unexpected empty list in apply"

-- Helper function to macro-expand an argument
macroExpand :: EnvRef -> AST -> Mal AST
macroExpand envRef ast@(ASTLV _ LVList (ASTSym s : otherArgs)) = do
  val <- Env.safeGet envRef s
  case val of
    Just (ASTFM _ FMMacro macroFn) -> do
      Debug.printInfo "macroExpanding" envRef ast
      retVal   <- macroFn otherArgs
      expanded <- macroExpand envRef retVal
      Debug.printInfo "macroExpanded" envRef expanded
      return expanded
    _ -> return ast
macroExpand _ ast = return ast
