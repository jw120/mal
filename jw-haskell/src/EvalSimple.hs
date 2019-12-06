{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : EvalSimple
Description : Simple version of AST evaluator
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Simplified version of the AST evaluator (with a fixed environment) for step 2

-}

module EvalSimple
  ( malEval
  )
where

import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

import qualified Builtin
import           Reader                         ( AST(..) )

-- | Fixed environment for the simple evaulator
simpleEnv :: Map Text AST
simpleEnv = M.fromList
  [ ("+", ASTBuiltin Builtin.addition)
  , ("-", ASTBuiltin Builtin.subtraction)
  , ("*", ASTBuiltin Builtin.multiplication)
  , ("/", ASTBuiltin Builtin.division)
  ]

-- | Top-level evaluator
--
-- Intercepts parsing errors (Left) or a missing (Nothing) and hands over to eval
malEval :: Either Text (Maybe AST) -> Either Text (Maybe AST)
malEval (Left  err       ) = Left err
malEval (Right Nothing   ) = Right Nothing
malEval (Right (Just ast)) = case eval ast of
  Left  err -> Left err
  Right x   -> Right (Just x)

-- | Main evaluation function - returns result (Right) or an error message (Left)
eval :: AST -> Either Text AST
eval (ASTList []           ) = Right (ASTList [])
eval (ASTList (func : args)) = do
  func' <- eval func
  args' <- mapM eval args
  case func' of
    ASTBuiltin b -> b args'
    _            -> Left "Not a function"
eval (ASTVector xs) = do
  xs' <- mapM eval xs
  return $ ASTVector xs'
eval (ASTMap m) = do
  elems' <- mapM eval $ M.elems m
  return . ASTMap . M.fromList $ zip (M.keys m) elems'
eval (ASTSym s) = case M.lookup s simpleEnv of
  Just v  -> Right v
  Nothing -> Left "Unknown symbol"
eval other = Right other
