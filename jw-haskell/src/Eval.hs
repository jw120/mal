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
                                                , EnvRef
                                                , Mal
                                                , Text
                                                , extractSym
                                                , MalError(..)
                                                )

-- | Main evaluation function
eval :: EnvRef -> AST -> Mal AST
eval envRef ast = do
  Debug.printInfo "eval" envRef ast
  case ast of
    ASTList []       -> return $ ASTList []
    ASTList listArgs -> do
      expansion <- macroExpand envRef (ASTList listArgs)
      case expansion of
        ASTList expansionListArgs -> do
          apply envRef expansionListArgs
        otherAst -> evalAst envRef otherAst
    _ -> evalAst envRef ast

-- Helper function for eval for non-lists
evalAst :: EnvRef -> AST -> Mal AST
evalAst envRef ast = do
  Debug.printInfo "evalAst" envRef ast
  case ast of
    ASTVector xs -> ASTVector <$> mapM (eval envRef) xs
    ASTMap    m  -> do
      elems' <- mapM (eval envRef) $ M.elems m
      return . ASTMap . M.fromList $ zip (M.keys m) elems'
    ASTSym s -> Env.get envRef s
    _        -> return ast

-- | Helper function apply to handle lists (mainly special forms)
apply :: EnvRef -> [AST] -> Mal AST
apply envRef astList = do
    Debug.printInfo "apply" envRef $ ASTList astList
    case astList of

        -- Special form: def!
        [ASTSym "def!", ASTSym sym, val] -> do
            val' <- eval envRef val
            Env.set envRef sym val'
            return val'
        (ASTSym "def!" : _) -> throwError $ EvalError "Bad syntax in def! special form"

        -- Special form: defmacro!
        [ASTSym "defmacro!", ASTSym sym, fnBody] -> do
            fn <- eval envRef fnBody
            case fn of
                ASTFunc _ f -> do
                    let macro = ASTFunc True f
                    Env.set envRef sym macro
                    return macro
                _ -> throwError $ EvalError "Expected a function in defmacro!"
        (ASTSym "defmacro!" : _) -> throwError $ EvalError "Bad syntax in defmacro! special form"

        -- Special form: do
        [ASTSym "do"] -> throwError $ EvalError "No arguments for do special form"
        (ASTSym "do" : args) -> last <$> mapM (eval envRef) args

        -- Special form: fn*
        [ASTSym "fn*", ASTList binds, body] -> do
            binds' <- mapM extractSym binds
            return . ASTFunc False $ closure binds'
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
                    Env.set e symVariadic (ASTList valVariadic)
                    return ()
                addBindingLists e (sym : symRest) (val : valRest) = do
                    Env.set e sym val
                    addBindingLists e symRest valRest
                addBindingLists _ [] [] = return ()
                addBindingLists _ _  _  = throwError $ EvalError "Unexpected value in add Bindings"
        [ASTSym "fn*", ASTVector binds, body] -> apply envRef [ASTSym "fn*", ASTList binds, body]
        (ASTSym "fn*" : _) -> throwError $ EvalError "Bad syntax in fn* special form"

        -- Special form: if
        [ASTSym "if", condArg, thenArg, elseArg] -> do
            condVal <- eval envRef condArg
            case condVal of
                ASTNil   -> eval envRef elseArg
                ASTFalse -> eval envRef elseArg
                _        -> eval envRef thenArg
        [ASTSym "if", condArg, thenArg] -> apply envRef [ASTSym "if", condArg, thenArg, ASTNil]
        (ASTSym "if" : _) -> throwError $ EvalError "Bad syntax in if special form"

        -- Special form: let*
        [ASTSym "let*", ASTList bindings, val] -> do
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
                addBindings _ _  = throwError $ EvalError "Unexpected value in bindings in let*"
        [ASTSym "let*", ASTVector bindings, val] -> apply envRef [ASTSym "let*", ASTList bindings, val]
        (ASTSym "let*" : _) -> throwError $ EvalError "Bad syntax in let* special form"

        -- Special form: macroexpand
        [ASTSym "macroexpand", ast] -> macroExpand envRef ast
        (ASTSym "macroexpand" : _) -> throwError $ EvalError "Bad syntax in macroexpand special form"

        -- Special form: quote
        [ASTSym "quote", val] -> return val
        (ASTSym "quote" : _) -> throwError $ EvalError "Bad syntax in quote special form"

        -- Special form: quasi-quote
        [ASTSym "quasiquote", ast] -> eval envRef $ quasiQuote ast
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
        (ASTSym "quasiquote" : _) -> throwError $ EvalError "Bad syntax in quasiquote special form"

        -- Special form: try*/catch*
        [ASTSym "try*", ast, ASTList[ASTSym "catch*", ASTSym exceptionVar, catchVal]] -> do
            eval envRef ast `catchError` handler
          where
            handler :: MalError -> Mal AST
            handler (ThrownError errAst) = do
                subEnvRef <- Env.new envRef
                Env.set subEnvRef exceptionVar errAst
                eval subEnvRef catchVal
            handler (ReaderError t) = do
                subEnvRef <- Env.new envRef
                Env.set subEnvRef exceptionVar $ ASTStr ("Reader Error: " <> t)
                eval subEnvRef catchVal
            handler (EvalError t) = do
                subEnvRef <- Env.new envRef
                Env.set subEnvRef exceptionVar $ ASTStr ("Evaluation Error: " <> t)
                eval subEnvRef catchVal
        (ASTSym "try*" : _) -> throwError $ EvalError "Bad try* syntax"
        (ASTSym "catch*" : _) -> throwError $ EvalError "Bad catch* syntax"

        -- If not a special form, then apply for a non-empty list
        (func : args) -> do
            func' <- eval envRef func
            case func' of
                ASTFunc _ f -> do
                    args' <- mapM (eval envRef) args
                    f args'
                _ -> throwError $ EvalError "Not a function"

        -- Empty list should not be passed into apply, but catch just in case
        [] -> throwError $ EvalError "Unexpected empty list in apply"

-- Helper function to macro-expand an argument
macroExpand :: EnvRef -> AST -> Mal AST
macroExpand envRef ast@(ASTList (ASTSym s : otherArgs)) = do
  val <- Env.safeGet envRef s
  case val of
    Just (ASTFunc True macroFn) -> do
      Debug.printInfo "macroExpanding" envRef ast
      retVal <- macroFn otherArgs
      expanded <- macroExpand envRef retVal
      Debug.printInfo "macroExpanded" envRef expanded
      return expanded
    _ -> return ast
macroExpand _ ast = return ast
