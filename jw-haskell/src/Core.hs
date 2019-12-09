{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Core
Description : Builtin functions for our intpreter
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Haskell implementations of builtins

-}

module Core
  ( nameSpace
  , prelude
  )
where

import           Control.Monad.Except

import           Data.Map                       ( Map )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Map                      as M

import           Types                          ( AST(..)
                                                , Mal
                                                , Text
                                                , astEquality
                                                )
import           Printer                        ( malFormat )

-- | Definitions read into Mal before execution of user program starts
prelude :: [Text]
prelude = ["(def! not (fn* (a) (if a false true)))"]

-- | Core name space which holds all of our built-ins
nameSpace :: Map Text AST
nameSpace = M.fromList
  [ ("+"      , ASTFunc addition)
  , ("-"      , ASTFunc subtraction)
  , ("*"      , ASTFunc multiplication)
  , ("/"      , ASTFunc division)
  , ("list"   , ASTFunc list)
  , ("count"  , ASTFunc count)
  , ("empty?" , ASTFunc emptyTest)
  , ("list?"  , ASTFunc listTest)
  , ("="      , ASTFunc equality)
  , (">"      , ASTFunc (binaryIntOp (>)))
  , (">="     , ASTFunc (binaryIntOp (>=)))
  , ("<"      , ASTFunc (binaryIntOp (<)))
  , ("<="     , ASTFunc (binaryIntOp (<=)))
  , ("pr-str" , ASTFunc prStr)
  , ("str"    , ASTFunc str)
  , ("prn"    , ASTFunc prn)
  , ("println", ASTFunc println)
  ]

addition :: [AST] -> Mal AST
addition asts = do
  xs <- mapM extractIntLit asts
  return $ ASTInt (sum xs)

subtraction :: [AST] -> Mal AST
subtraction [] = throwError "Arugment error: at least one argument required"
subtraction [ASTInt i ] = return (ASTInt (-i))
subtraction [_        ] = throwError "Type error: integer expected"
subtraction (hd : rest) = do
  hd'   <- extractIntLit hd
  rest' <- mapM extractIntLit rest
  return . ASTInt $ hd' - sum rest'

multiplication :: [AST] -> Mal AST
multiplication asts = do
  xs <- mapM extractIntLit asts
  return $ ASTInt (product xs)

division :: [AST] -> Mal AST
division [] = throwError "Arugment error: at least one argument required"
division [ASTInt _] =
  throwError "Arugment error: at least one argument required"
division [_        ] = throwError "Type error: integer expected"
division (hd : rest) = do
  hd'   <- extractIntLit hd
  rest' <- mapM extractIntLit rest
  hd' `safeDiv` product rest'

safeDiv :: Int -> Int -> Mal AST
safeDiv _ 0 = throwError "Division by zero error"
safeDiv i j = return . ASTInt $ i `div` j

list :: [AST] -> Mal AST
list = return . ASTList

listTest :: [AST] -> Mal AST
listTest (ASTList _ : _) = return ASTTrue
listTest _               = return ASTFalse

count :: [AST] -> Mal AST
count (ASTList   xs : _) = return (ASTInt (length xs))
count (ASTVector xs : _) = return (ASTInt (length xs))
count _                  = return (ASTInt 0)

emptyTest :: [AST] -> Mal AST
emptyTest (ASTList   [] : _) = return ASTTrue
emptyTest (ASTVector [] : _) = return ASTTrue
emptyTest _                  = return ASTFalse

equality :: [AST] -> Mal AST
equality []  = throwError "Expecting two arguments for equality testing"
equality [_] = throwError "Expecting two arguments for equality testing"
equality (a : b : _) | a `astEquality` b = return ASTTrue
                     | otherwise         = return ASTFalse

-- Helper function to convert <, > etc into the correct format
binaryIntOp :: (Int -> Int -> Bool) -> [AST] -> Mal AST
binaryIntOp _ []  = throwError "Expecting two arguments"
binaryIntOp _ [_] = throwError "Expecting two arguments"
binaryIntOp op (ASTInt a : ASTInt b : _) | a `op` b  = return ASTTrue
                                         | otherwise = return ASTFalse
binaryIntOp _ _ = return ASTFalse

prStr :: [AST] -> Mal AST
prStr = return . ASTStr . T.intercalate " " . map (malFormat True)

str :: [AST] -> Mal AST
str = return . ASTStr . T.concat . map (malFormat False)

prn :: [AST] -> Mal AST
prn xs = do
  let s = T.intercalate " " $ map (malFormat True) xs
  liftIO $ TIO.putStrLn s
  return ASTNil

println :: [AST] -> Mal AST
println xs = do
  let s = T.intercalate " " $ map (malFormat False) xs
  liftIO $ TIO.putStrLn s
  return ASTNil

-- Helper function to convert a list of IntLits to Int (or return Nothing if a type error)
extractIntLit :: AST -> Mal Int
extractIntLit (ASTInt i) = return i
extractIntLit _          = throwError "Type error: integer expected"


