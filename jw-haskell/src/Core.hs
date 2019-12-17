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

import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Map                      as M

import           Types                          ( AST(..)
                                                , EnvRef
                                                , Mal
                                                , Text
                                                , astEquality
                                                , extractInt
                                                )
import           Printer                        ( malFormat )
import           Reader                         ( malRead )
import           Eval                           ( eval )


-- | Definitions read into Mal before execution of user program starts
prelude :: [Text]
prelude =
  [ "(def! not (fn* (a) (if a false true)))"
  , "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\"))))))"
  ]

-- | Core name space which holds all of our built-ins, takes top-level REPL environment for eval
nameSpace :: EnvRef -> Map Text AST
nameSpace envRef = M.fromList
  [ ("+"          , ASTFunc addition)
  , ("-"          , ASTFunc subtraction)
  , ("*"          , ASTFunc multiplication)
  , ("/"          , ASTFunc division)
  , ("list"       , ASTFunc list)
  , ("count"      , ASTFunc count)
  , ("empty?"     , ASTFunc emptyTest)
  , ("list?"      , ASTFunc listTest)
  , ("="          , ASTFunc equality)
  , (">"          , ASTFunc (binaryIntOp (>)))
  , (">="         , ASTFunc (binaryIntOp (>=)))
  , ("<"          , ASTFunc (binaryIntOp (<)))
  , ("<="         , ASTFunc (binaryIntOp (<=)))
  , ("pr-str"     , ASTFunc prStr)
  , ("str"        , ASTFunc str)
  , ("prn"        , ASTFunc prn)
  , ("println"    , ASTFunc println)
  , ("read-string", ASTFunc readString)
  , ("slurp"      , ASTFunc slurp)
  , ("eval"       , ASTFunc (replEval envRef))
  , ("atom"       , ASTFunc atom)
  , ("atom?"      , ASTFunc atomTest)
  , ("deref"      , ASTFunc deref)
  , ("reset!"     , ASTFunc reset)
  , ("swap!"      , ASTFunc (swap envRef))
  , ("cons"       , ASTFunc cons)
  , ("concat"     , ASTFunc malConcat)
  ]


addition :: [AST] -> Mal AST
addition asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (sum xs)

subtraction :: [AST] -> Mal AST
subtraction [] = throwError "Arugment error: at least one argument required"
subtraction [ASTInt i ] = return (ASTInt (-i))
subtraction [_        ] = throwError "Type error: integer expected"
subtraction (hd : rest) = do
  hd'   <- extractInt hd
  rest' <- mapM extractInt rest
  return . ASTInt $ hd' - sum rest'

multiplication :: [AST] -> Mal AST
multiplication asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (product xs)

division :: [AST] -> Mal AST
division [] = throwError "Arugment error: at least one argument required"
division [ASTInt _] =
  throwError "Arugment error: at least one argument required"
division [_        ] = throwError "Type error: integer expected"
division (hd : rest) = do
  hd'   <- extractInt hd
  rest' <- mapM extractInt rest
  hd' `safeDiv` product rest'

safeDiv :: Int -> Int -> Mal AST
safeDiv _ 0 = throwError "Division by zero error"
safeDiv i j = return . ASTInt $ i `div` j

-- Helper function to convert <, > etc into the correct format
binaryIntOp :: (Int -> Int -> Bool) -> [AST] -> Mal AST
binaryIntOp _ []  = throwError "Expecting two arguments"
binaryIntOp _ [_] = throwError "Expecting two arguments"
binaryIntOp op (ASTInt a : ASTInt b : _) | a `op` b  = return ASTTrue
                                         | otherwise = return ASTFalse
binaryIntOp _ _ = return ASTFalse

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

prStr :: [AST] -> Mal AST
prStr xs = do
  xs' <- liftIO $ mapM (malFormat True) xs
  return . ASTStr $ T.intercalate " " xs'

str :: [AST] -> Mal AST
str xs = do
  xs' <- liftIO $ mapM (malFormat False) xs
  return . ASTStr $ T.concat xs'

prn :: [AST] -> Mal AST
prn xs = do
  xs' <- liftIO $ mapM (malFormat True) xs
  let s = T.intercalate " " xs'
  liftIO $ TIO.putStrLn s
  return ASTNil

println :: [AST] -> Mal AST
println xs = do
  xs' <- liftIO $ mapM (malFormat False) xs
  let s = T.intercalate " " xs'
  liftIO $ TIO.putStrLn s
  return ASTNil

readString :: [AST] -> Mal AST
readString [ASTStr s] = case malRead s of
  Left  err        -> throwError err
  Right Nothing    -> return ASTNil
  Right (Just ast) -> return ast
readString _ = throwError "Bad argument type for readString"

slurp :: [AST] -> Mal AST
slurp [ASTStr fn] = do
  s <- liftIO . TIO.readFile $ T.unpack fn
  return $ ASTStr s
slurp _ = throwError "Bad argument type for slurp"

replEval :: EnvRef -> [AST] -> Mal AST
replEval replEnvRef [ast] = eval replEnvRef ast
replEval _          _     = throwError "Bad argument for eval"

atom :: [AST] -> Mal AST
atom [ast] = do
  ref <- liftIO $ newIORef ast
  return $ ASTAtom ref
atom _ = throwError "Bad arguments for atom"

atomTest :: [AST] -> Mal AST
atomTest [ASTAtom _] = return ASTTrue
atomTest [_        ] = return ASTFalse
atomTest _           = throwError "Bad arguments for atom?"

deref :: [AST] -> Mal AST
deref [ASTAtom ref] = liftIO $ readIORef ref
deref _             = throwError "Bad arguments for deref"

reset :: [AST] -> Mal AST
reset [ASTAtom ref, val] = do
  liftIO $ writeIORef ref val
  return val
reset _ = throwError "Bad arguments for reset"

swap :: EnvRef -> [AST] -> Mal AST
swap envRef (ASTAtom ref : ASTFunc func : args) = do
  val  <- liftIO $ readIORef ref
  val' <- eval envRef $ ASTList (ASTFunc func : val : args)
  liftIO $ writeIORef ref val'
  return val'
swap _ _ = throwError "Bad arguments for swap"

cons :: [AST] -> Mal AST
cons [ast, ASTList xs] = return $ ASTList (ast : xs)
cons _                 = throwError "Bad arguments for cons"

malConcat :: [AST] -> Mal AST
malConcat (ASTList xs : ASTList ys : rest) =
  malConcat (ASTList (xs ++ ys) : rest)
malConcat [ASTList xs] = return $ ASTList xs
malConcat []           = return $ ASTList []
malConcat _            = throwError "Bad arguments for concat"
