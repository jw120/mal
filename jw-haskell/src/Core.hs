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
                                                , magicKeywordPrefix
                                                , astEquality
                                                , extractInt
                                                , MalError(..)
                                                , throwString
                                                )
import           Printer                        ( malFormat )
import           Reader                         ( malRead )
import           Eval                           ( eval )


-- | Definitions read into Mal before execution of user program starts
prelude :: [Text]
prelude =
  [ "(def! not (fn* (a) (if a false true)))"
  , "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\"))))))"
  , "(defmacro! cond (fn* (& xs) "
    <> "(if (> (count xs) 0) "
    <> "(list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) "
    <> "(cons 'cond (rest (rest xs)))))))"
  ]

-- | Core name space which holds all of our built-ins, takes top-level REPL environment for eval
nameSpace :: EnvRef -> Map Text AST
nameSpace envRef = M.fromList
  [ ("+"           , ASTFunc False addition)
  , ("-"           , ASTFunc False subtraction)
  , ("*"           , ASTFunc False multiplication)
  , ("/"           , ASTFunc False division)
  , ("list"        , ASTFunc False list)
  , ("count"       , ASTFunc False count)
  , ("empty?"      , ASTFunc False emptyTest)
  , ("list?"       , ASTFunc False listTest)
  , ("="           , ASTFunc False equality)
  , (">"           , ASTFunc False (binaryIntOp (>)))
  , (">="          , ASTFunc False (binaryIntOp (>=)))
  , ("<"           , ASTFunc False (binaryIntOp (<)))
  , ("<="          , ASTFunc False (binaryIntOp (<=)))
  , ("pr-str"      , ASTFunc False prStr)
  , ("str"         , ASTFunc False str)
  , ("prn"         , ASTFunc False prn)
  , ("println"     , ASTFunc False println)
  , ("read-string" , ASTFunc False readString)
  , ("slurp"       , ASTFunc False slurp)
  , ("eval"        , ASTFunc False (replEval envRef))
  , ("atom"        , ASTFunc False atom)
  , ("atom?"       , ASTFunc False atomTest)
  , ("deref"       , ASTFunc False deref)
  , ("reset!"      , ASTFunc False reset)
  , ("swap!"       , ASTFunc False (swap envRef))
  , ("cons"        , ASTFunc False cons)
  , ("concat"      , ASTFunc False malConcat)
  , ("nth"         , ASTFunc False nth)
  , ("first"       , ASTFunc False first)
  , ("rest"        , ASTFunc False rest)
  , ("throw"       , ASTFunc False malThrow)
  , ("apply"       , ASTFunc False apply)
  , ("map"         , ASTFunc False malMap)
  , ("nil?"        , ASTFunc False $ malIs ASTNil)
  , ("true?"       , ASTFunc False $ malIs ASTTrue)
  , ("false?"      , ASTFunc False $ malIs ASTFalse)
  , ("symbol?"     , ASTFunc False symbolTest)
  , ("symbol"      , ASTFunc False symbol)
  , ("keyword"     , ASTFunc False keyword)
  , ("keyword?"    , ASTFunc False keywordTest)
  , ("vector"      , ASTFunc False vector)
  , ("vector?"     , ASTFunc False vectorTest)
  , ("sequential?" , ASTFunc False sequentialTest)
  , ("hash-map"    , ASTFunc False hashMap)
  , ("map?"        , ASTFunc False mapTest)
  , ("assoc"       , ASTFunc False assoc)
  , ("dissoc"      , ASTFunc False dissoc)
  , ("get"         , ASTFunc False get)
  , ("contains?"   , ASTFunc False containsTest)
  , ("keys"        , ASTFunc False keys)
  , ("vals"        , ASTFunc False vals)
  ]


addition :: [AST] -> Mal AST
addition asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (sum xs)

subtraction :: [AST] -> Mal AST
subtraction [] = throwString "Argument error: at least one argument required"
subtraction [ASTInt i] = return (ASTInt (-i))
subtraction [_       ] = throwString "Type error: integer expected"
subtraction (hd : tl ) = do
  hd' <- extractInt hd
  tl' <- mapM extractInt tl
  return . ASTInt $ hd' - sum tl'

multiplication :: [AST] -> Mal AST
multiplication asts = do
  xs <- mapM extractInt asts
  return $ ASTInt (product xs)

division :: [AST] -> Mal AST
division [] = throwString "Arugment error: at least one argument required"
division [ASTInt _] =
  throwString "Arugment error: at least one argument required"
division [_      ] = throwString "Type error: integer expected"
division (hd : tl) = do
  hd' <- extractInt hd
  tl' <- mapM extractInt tl
  hd' `safeDiv` product tl'

safeDiv :: Int -> Int -> Mal AST
safeDiv _ 0 = throwString "Division by zero error"
safeDiv i j = return . ASTInt $ i `div` j

-- Helper function to convert <, > etc into the correct format
binaryIntOp :: (Int -> Int -> Bool) -> [AST] -> Mal AST
binaryIntOp _ []  = throwString "Expecting two arguments"
binaryIntOp _ [_] = throwString "Expecting two arguments"
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
equality []  = throwString "Expecting two arguments for equality testing"
equality [_] = throwString "Expecting two arguments for equality testing"
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
  Left  err        -> throwString err
  Right Nothing    -> return ASTNil
  Right (Just ast) -> return ast
readString _ = throwString "Bad argument type for readString"

slurp :: [AST] -> Mal AST
slurp [ASTStr fn] = do
  s <- liftIO . TIO.readFile $ T.unpack fn
  return $ ASTStr s
slurp _ = throwString "Bad argument type for slurp"

replEval :: EnvRef -> [AST] -> Mal AST
replEval replEnvRef [ast] = eval replEnvRef ast
replEval _          _     = throwString "Bad argument for eval"

atom :: [AST] -> Mal AST
atom [ast] = do
  ref <- liftIO $ newIORef ast
  return $ ASTAtom ref
atom _ = throwString "Bad arguments for atom"

atomTest :: [AST] -> Mal AST
atomTest [ASTAtom _] = return ASTTrue
atomTest [_        ] = return ASTFalse
atomTest _           = throwString "Bad arguments for atom?"

deref :: [AST] -> Mal AST
deref [ASTAtom ref] = liftIO $ readIORef ref
deref _             = throwString "Bad arguments for deref"

reset :: [AST] -> Mal AST
reset [ASTAtom ref, val] = do
  liftIO $ writeIORef ref val
  return val
reset _ = throwString "Bad arguments for reset"

swap :: EnvRef -> [AST] -> Mal AST
swap envRef (ASTAtom ref : ASTFunc False func : args) = do
  val  <- liftIO $ readIORef ref
  val' <- eval envRef $ ASTList (ASTFunc False func : val : args)
  liftIO $ writeIORef ref val'
  return val'
swap _ _ = throwString "Bad arguments for swap"

cons :: [AST] -> Mal AST
cons [ast, ASTList xs  ] = return $ ASTList (ast : xs)
cons [ast, ASTVector xs] = return $ ASTList (ast : xs)
cons _                   = throwString "Bad arguments for cons"

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
malConcat _              = throwString "Bad arguments for concat"


nth :: [AST] -> Mal AST
nth [ASTList ys, ASTInt i]
  | i >= 0 && i < length ys = return $ ys !! i
  | otherwise               = throwString "Argument out of bounds in nth"
nth [ASTVector ys, ASTInt i] | i >= 0 && i < length ys = return $ ys !! i
                             | otherwise = throwString "Bad argument in nth"
nth _ = throwString "Bad arguments for nth"

first :: [AST] -> Mal AST
first [ASTList   (x : _)] = return x
first [ASTList   []     ] = return ASTNil
first [ASTVector (x : _)] = return x
first [ASTVector []     ] = return ASTNil
first [ASTNil           ] = return ASTNil
first _                   = throwString "Bad arguments for first"

rest :: [AST] -> Mal AST
rest [ASTList   (_ : ys)] = return $ ASTList ys
rest [ASTList   []      ] = return $ ASTList []
rest [ASTVector (_ : ys)] = return $ ASTList ys
rest [ASTVector []      ] = return $ ASTList []
rest [ASTNil            ] = return $ ASTList []
rest _                    = throwString "Bad arguments for rest"

malThrow :: [AST] -> Mal AST
malThrow [ast] = throwError $ MalError ast
malThrow _     = throwString "Bad arguments for throw"

apply :: [AST] -> Mal AST
apply [ASTFunc False _] = throwString "No arguments to apply"
apply (ASTFunc False fn : args) = case last args of
        ASTList lastList -> fn $ init args ++ lastList
        ASTVector lastList -> fn $ init args ++ lastList
        _ -> throwString "Expected list as final argument for apply"
apply (ASTFunc True _ : _) = throwString "Attempt to apply a macro"
apply _ = throwString "Bad arguments for apply"

malMap :: [AST] -> Mal AST
malMap [ASTFunc False fn, ASTList xs] = ASTList <$> mapM (\x -> fn [x]) xs
malMap [ASTFunc False fn, ASTVector xs] = malMap [ASTFunc False fn, ASTList xs]
malMap [ASTFunc True _, ASTList _] = throwString "Attempt to map a macro"
malMap _ = throwString "Bad arguments for map"

malIs :: AST -> [AST] -> Mal AST
malIs target [ast]
    | target == ast = return ASTTrue
    | otherwise = return ASTFalse
malIs _ _ = throwString "Expected one argument"

symbolTest :: [AST] -> Mal AST
symbolTest [ASTSym _] = return ASTTrue
symbolTest [_] = return ASTFalse
symbolTest _ = throwString "Expected one argument for symbol?"

symbol :: [AST] -> Mal AST
symbol [ASTStr s] = return $ ASTSym s
symbol _ = throwString "Expected one string for symbol"

keyword :: [AST] -> Mal AST
keyword [ASTStr s]
    | T.isPrefixOf magicKeywordPrefix s = return $ ASTStr s
    | otherwise = return . ASTStr $ magicKeywordPrefix <> s
keyword _ = throwString "Expected one string for keyword"

keywordTest :: [AST] -> Mal AST
keywordTest [ASTStr s]
    | T.isPrefixOf magicKeywordPrefix s = return ASTTrue
    | otherwise = return ASTFalse
keywordTest [_] = return ASTFalse
keywordTest _ = throwString "Expected one argument for keyword?"

vector :: [AST] -> Mal AST
vector = return . ASTVector

vectorTest :: [AST] -> Mal AST
vectorTest [ASTVector _] = return ASTTrue
vectorTest [_] = return ASTFalse
vectorTest _ = throwString "Expected one argument for vector?"

sequentialTest :: [AST] -> Mal AST
sequentialTest [ASTList _] = return ASTTrue
sequentialTest [ASTVector _] = return ASTTrue
sequentialTest [_] = return ASTFalse
sequentialTest _ = throwString "Expected one argument for sequential?"

hashMap :: [AST] -> Mal AST
hashMap xs = do
    xs' <- alternatingToPairs xs
    return . ASTMap $ M.fromList xs'
  where

-- Helper function
alternatingToPairs :: [AST] -> Mal [(Text, AST)]
alternatingToPairs (ASTStr a : b : cs) = do
    cs' <- alternatingToPairs cs
    return $ (a, b) : cs'
alternatingToPairs (_ : _ : _ ) = throwString "Expected string or keyword for map associations"
alternatingToPairs [_] = throwString "Expected even number of arguments for map associatons"
alternatingToPairs [] = return []

mapTest :: [AST] -> Mal AST
mapTest [ASTMap _] = return ASTTrue
mapTest [_] = return ASTFalse
mapTest _ = throwString "Expected one argument for map?"

assoc :: [AST] -> Mal AST
assoc (ASTMap m : newArgs) = do
    newPairs <- alternatingToPairs newArgs
    let newMap = M.fromList newPairs
    return . ASTMap $ M.union newMap m
assoc _ = throwString "NYI"

dissoc :: [AST] -> Mal AST
dissoc (ASTMap m : ASTStr x : ys) = dissoc (ASTMap (M.delete x m) :  ys)
dissoc [ASTMap m] = return $ ASTMap m
dissoc _ = throwString "Bad arguments for dissoc"

get :: [AST] -> Mal AST
get [ASTNil, ASTStr _] = return ASTNil
get [ASTMap m, ASTStr k] = case M.lookup k m of
    Just v -> return v
    Nothing -> return ASTNil
get _ = throwString "Bad arguments for get"

containsTest :: [AST] -> Mal AST
containsTest [ASTMap m, ASTStr k]
    | M.member k m = return ASTTrue
    | otherwise = return ASTFalse
containsTest _ = throwString "Bad arguments for get"

keys :: [AST] -> Mal AST
keys [ASTMap m] = return . ASTList . map ASTStr $ M.keys m
keys _ = throwString "keys expects a hash-map"

vals :: [AST] -> Mal AST
vals [ASTMap m] = return . ASTList $ M.elems m
vals _ = throwString "vals expects a hash-map"
