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
  )
where

import qualified Data.Map                      as M
import           Data.Text                      ( Text )

import qualified Builtin
import           Env
import           Reader                         ( AST(..) )

simpleEnv :: Env
simpleEnv =
  set "+" (ASTBuiltin Builtin.addition)
    . set "-" (ASTBuiltin Builtin.subtraction)
    . set "*" (ASTBuiltin Builtin.multiplication)
    $ set "/" (ASTBuiltin Builtin.division) emptyWithoutOuter

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
eval (ASTVector xs) = ASTVector <$> mapM eval xs
eval (ASTMap    m ) = do
  elems' <- mapM eval $ M.elems m
  return . ASTMap . M.fromList $ zip (M.keys m) elems'
eval (ASTSym s) = get s simpleEnv
eval other      = Right other
