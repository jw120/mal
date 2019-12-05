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

import           Data.Either                    ( partitionEithers )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

import           Reader                         ( AST(..)
                                                , MalBuiltin
                                                )

wrapIntBinary :: (Int -> Int -> Int) -> MalBuiltin
wrapIntBinary binFn = f
 where
  f [ASTIntLit i, ASTIntLit j] = Right (ASTIntLit (binFn i j))
  f [_          , _          ] = Left "Expected integer arguments"
  f _                          = Left "Expected two arguments"

replEnv :: Map Text AST
replEnv = M.fromList
  [ ("+", ASTBuiltin (wrapIntBinary (+)))
  , ("-", ASTBuiltin (wrapIntBinary (-)))
  , ("*", ASTBuiltin (wrapIntBinary (*)))
  , ("/", ASTBuiltin (wrapIntBinary div))
  ]

malEval :: Either Text (Maybe AST) -> Either Text (Maybe AST)
malEval (Left  err       ) = Left err
malEval (Right Nothing   ) = Right Nothing
malEval (Right (Just ast)) = case eval ast of
  Left  err -> Left err
  Right x   -> Right (Just x)

eval :: AST -> Either Text AST
eval (ASTList []) = Right (ASTList [])
eval (ASTList xs)
  | not (null lefts) = Left (head lefts)
  | otherwise = case hd of
    ASTBuiltin fn -> fn rest
    _             -> Left "Not a function"
 where
  xs'                = map eval xs
  (lefts, hd : rest) = partitionEithers xs'
eval (ASTVector xs) | null lefts = Right (ASTVector rights)
                    | otherwise  = Left (head lefts)
 where
  xs'             = map eval xs
  (lefts, rights) = partitionEithers xs'
-- eval (ASTMap ) TBD
eval (ASTSymbol s) = case M.lookup s replEnv of
  Just v  -> Right v
  Nothing -> Left "Look up failed"
eval other = Right other

