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
  , "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
  ]

-- | Core name space which holds all of our built-ins, takes top-level REPL environment for eval
nameSpace :: EnvRef -> Map Text AST
nameSpace envRef = M.fromList
  [ ("+"          , ASTFunc False addition)
  , ("-"          , ASTFunc False subtraction)
  , ("*"          , ASTFunc False multiplication)
  , ("/"          , ASTFunc False division)
  , ("list"       , ASTFunc False list)
  , ("count"      , ASTFunc False count)
  , ("empty?"     , ASTFunc False emptyTest)
  , ("list?"      , ASTFunc False listTest)
  , ("="          , ASTFunc False equality)
  , (">"          , ASTFunc False (binaryIntOp (>)))
  , (">="         , ASTFunc False (binaryIntOp (>=)))
  , ("<"          , ASTFunc False (binaryIntOp (<)))
  , ("<="         , ASTFunc False (binaryIntOp (<=)))
  , ("pr-str"     , ASTFunc False prStr)
  , ("str"        , ASTFunc False str)
  , ("prn"        , ASTFunc False prn)
  , ("println"    , ASTFunc False println)
  , ("read-string", ASTFunc False readString)
  , ("slurp"      , ASTFunc False slurp)
  , ("eval"       , ASTFunc False (replEval envRef))
  , ("atom"       , ASTFunc False atom)
  , ("atom?"      , ASTFunc False atomTest)
  , ("deref"      , ASTFunc False deref)
  , ("reset!"     , ASTFunc False reset)
  , ("swap!"      , ASTFunc False (swap envRef))
  , ("cons"       , ASTFunc False cons)
  , ("concat"     , ASTFunc False malConcat)
  , ("nth"        , ASTFunc False nth)
  , ("first"      , ASTFunc False first)
  , ("rest"       , ASTFunc False rest)
  ]


addition :: [AST] -> Mal AST
addition asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (sum xs)

subtraction :: [AST] -> Mal AST
subtraction [] = throwError "Arugment error: at least one argument required"
subtraction [ASTInt i] = return (ASTInt (-i))
subtraction [_       ] = throwError "Type error: integer expected"
subtraction (hd : tl ) = do
  hd' <- extractInt hd
  tl' <- mapM extractInt tl
  return . ASTInt $ hd' - sum tl'

multiplication :: [AST] -> Mal AST
multiplication asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (product xs)

division :: [AST] -> Mal AST
division [] = throwError "Arugment error: at least one argument required"
division [ASTInt _] =
  throwError "Arugment error: at least one argument required"
division [_      ] = throwError "Type error: integer expected"
division (hd : tl) = do
  hd' <- extractInt hd
  tl' <- mapM extractInt tl
  hd' `safeDiv` product tl'

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
  printWithSpaces xs'

println :: [AST] -> Mal AST
println xs = do
  xs' <- liftIO $ mapM (malFormat False) xs
  printWithSpaces xs'

-- Helper function
printWithSpaces :: [Text] -> Mal AST
printWithSpaces texts = do
  let s = T.intercalate " " texts
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
swap envRef (ASTAtom ref : ASTFunc False func : args) = do
  val  <- liftIO $ readIORef ref
  val' <- eval envRef $ ASTList (ASTFunc False func : val : args)
  liftIO $ writeIORef ref val'
  return val'
swap _ _ = throwError "Bad arguments for swap"

cons :: [AST] -> Mal AST
cons [ast, ASTList xs  ] = return $ ASTList (ast : xs)
cons [ast, ASTVector xs] = return $ ASTList (ast : xs)
cons _                   = throwError "Bad arguments for cons"

malConcat :: [AST] -> Mal AST
malConcat (ASTList xs : ASTList ys : zs) = malConcat (ASTList (xs ++ ys) : zs)
malConcat (ASTList xs : ASTVector ys : zs) =
  malConcat (ASTList (xs ++ ys) : zs)
malConcat (ASTVector xs : ASTList ys : zs) =
  malConcat (ASTList (xs ++ ys) : zs)
malConcat (ASTVector xs : ASTVector ys : zs) =
  malConcat (ASTList (xs ++ ys) : zs)
malConcat [ASTList   xs] = return $ ASTList xs
malConcat [ASTVector xs] = return $ ASTList xs
malConcat []             = return $ ASTList []
malConcat _              = throwError "Bad arguments for concat"


nth :: [AST] -> Mal AST
nth [ASTList ys, ASTInt i] | i >= 0 && i < length ys = return $ ys !! i
                           | otherwise = throwError "Bad argument in nth"
nth [ASTVector ys, ASTInt i] | i >= 0 && i < length ys = return $ ys !! i
                             | otherwise = throwError "Bad argument in nth"
nth _ = throwError "Bad arguments for nth"

first :: [AST] -> Mal AST
first [ASTList   (x : _)] = return x
first [ASTList   []     ] = return ASTNil
first [ASTVector (x : _)] = return x
first [ASTVector []     ] = return ASTNil
first [ASTNil           ] = return ASTNil
first _                   = throwError "Bad arguments for first"

rest :: [AST] -> Mal AST
rest [ASTList   (_ : ys)] = return $ ASTList ys
rest [ASTList   []      ] = return $ ASTList []
rest [ASTVector (_ : ys)] = return $ ASTList ys
rest [ASTVector []      ] = return $ ASTList []
rest [ASTNil            ] = return $ ASTList []
rest _                    = throwError "Bad arguments for rest"
