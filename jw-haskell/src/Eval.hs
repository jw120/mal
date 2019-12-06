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

import qualified Builtin
import           Env                            ( Env )
import qualified Env                           as E
import           Reader                         ( AST(..) )

malInitialEnv :: Env
malInitialEnv =
  E.set "+" (ASTBuiltin Builtin.addition)
    . E.set "-" (ASTBuiltin Builtin.subtraction)
    . E.set "*" (ASTBuiltin Builtin.multiplication)
    $ E.set "/" (ASTBuiltin Builtin.division) E.emptyWithoutOuter


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

-- Evaluation for a non-empty list
eval (ASTList (func : args)) = do
  func' <- eval func
  args' <- mapM eval args
  case func' of
    ASTBuiltin b -> liftEither $ b args'
    _            -> throwError "Not a function"

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
