{-# LANGUAGE LambdaCase #-}
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
  , replPrelude
  )
where

import           Control.Monad.Except
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Map                      as M
import           System.Console.Readline        ( readline )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )


import           Types                          ( AST(..)
                                                , FMType(..)
                                                , LVType(..)
                                                , EnvRef
                                                , Mal
                                                , Text
                                                , magicKeywordPrefix
                                                , collectionEquality
                                                , extractInt
                                                , boolToAST
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
  , "(def! *host-language* \"jw-haskell\")"
  ]

-- | Extra definitions read after the prelude when started in repl mode
replPrelude :: [Text]
replPrelude = ["(println (str \"Mal [\" *host-language* \"]\"))"]

--
-- Helper functions to shorten envRef definition
--

-- Helper for function that already operate on a list
fn :: ([AST] -> Mal AST) -> AST
fn = ASTFM ASTNil FMFunction

-- Helper for functions that operates on one argument
fn1 :: (AST -> Mal AST) -> AST
fn1 f = ASTFM ASTNil FMFunction g
 where
  g :: [AST] -> Mal AST
  g [x] = f x
  g _   = throwString "Expected one argument"

-- Helper for pure functions that operates on one string
fn1s :: (Text -> AST) -> AST
fn1s f = ASTFM ASTNil FMFunction g
 where
  g :: [AST] -> Mal AST
  g [ASTStr s] = return $ f s
  g _          = throwString "Expected one string argument"

-- Helper for functions that runs a predicate on a single argument
fn1m :: (AST -> Bool) -> AST
fn1m matcher = fn1 $ return . (\x -> if matcher x then ASTTrue else ASTFalse)

-- Helper for pure functions that operates on two arguments
fn2 :: (AST -> AST -> AST) -> AST
fn2 f = ASTFM ASTNil FMFunction g
 where
  g :: [AST] -> Mal AST
  g [x, y] = return $ f x y
  g _      = throwString "Expected two arguments"

-- Helper for pure functions that operate on two ints
fn2i :: (Int -> Int -> AST) -> AST
fn2i f = ASTFM ASTNil FMFunction g
 where
  g :: [AST] -> Mal AST
  g [ASTInt a, ASTInt b] = return $ f a b
  g [_       , _       ] = throwString "Expected integer arguments"
  g _                    = throwString "Expected two arguments"

-- | Core name space which holds all of our built-ins, takes top-level REPL environment for eval
nameSpace :: EnvRef -> Map Text AST
nameSpace envRef = M.fromList
  [

    -- Basic arithmetic functions
    ("+", fn addition)
  , ("-", fn subtraction)
  , ("*", fn multiplication)
  , ( "/"
    , fn division
    )

    -- Matching functions
  , ( "list?"
    , fn1m
      (\case
        ASTLV _ LVList _ -> True
        _                -> False
      )
    )
  , ( "atom?"
    , fn1m
      (\case
        ASTAtom _ -> True
        _         -> False
      )
    )
  , ( "nil?"
    , fn1m
      (\case
        ASTNil -> True
        _      -> False
      )
    )
  , ( "true?"
    , fn1m
      (\case
        ASTTrue -> True
        _       -> False
      )
    )
  , ( "false?"
    , fn1m
      (\case
        ASTFalse -> True
        _        -> False
      )
    )
  , ( "symbol?"
    , fn1m
      (\case
        ASTSym _ -> True
        _        -> False
      )
    )
  , ( "vector?"
    , fn1m
      (\case
        ASTLV _ LVVector _ -> True
        _                  -> False
      )
    )
  , ( "map?"
    , fn1m
      (\case
        ASTMap _ _ -> True
        _          -> False
      )
    )
  , ( "number?"
    , fn1m
      (\case
        ASTInt _ -> True
        _        -> False
      )
    )
  , ( "fn?"
    , fn1m
      (\case
        ASTFM _ FMFunction _ -> True
        _                    -> False
      )
    )
  , ( "macro?"
    , fn1m
      (\case
        ASTFM _ FMMacro _ -> True
        _                 -> False
      )
    )
  , ( "sequential?"
    , fn1m
      (\case
        ASTLV{} -> True
        _       -> False
      )
    )
  , ( "empty?"
    , fn1m
      (\case
        ASTLV _ _ [] -> True
        _            -> False
      )
    )
  , ( "string?"
    , fn1m
      (\case
        ASTStr s -> not (T.isPrefixOf magicKeywordPrefix s)
        _        -> False
      )
    )
  , ( "keyword?"
    , fn1m
      (\case
        ASTStr s -> T.isPrefixOf magicKeywordPrefix s
        _        -> False
      )
    )

    -- Comparison operators (work only on numbers)
  , (">" , fn2i (\x y -> boolToAST (x > y)))
  , (">=", fn2i (\x y -> boolToAST (x >= y)))
  , ("<" , fn2i (\x y -> boolToAST (x < y)))
  , ( "<="
    , fn2i (\x y -> boolToAST (x <= y))
    )

    -- Utility functions
  , ("=", fn2 (\x y -> boolToAST (x `collectionEquality` y)))
  , ("symbol", fn1s ASTSym)
  , ( "keyword"
    , fn1s
    $ ASTStr
    . (\s -> if T.isPrefixOf magicKeywordPrefix s
        then s
        else magicKeywordPrefix <> s
      )
    )
  , ("eval"   , fn1 (eval envRef))
  , ("apply"  , fn apply)
  , ("throw"  , fn malThrow)
  , ("time-ms", fn timeMS)
  , ("meta"   , fn nyi)
  , ( "with-meta"
    , fn nyi
    )

  -- IO functions
  , ("pr-str"     , fn prStr)
  , ("str"        , fn str)
  , ("prn"        , fn prn)
  , ("println"    , fn println)
  , ("read-string", fn readString)
  , ("readline"   , fn malReadline)
  , ( "slurp"
    , fn slurp
    )

    -- Sequence functions
  , ("list", fn $ \xs -> return (ASTLV ASTNil LVList xs))
  , ("vector", fn $ \xs -> return (ASTLV ASTNil LVVector xs))
  , ("count" , fn count)
  , ("cons"  , fn cons)
  , ("concat", fn malConcat)
  , ("nth"   , fn nth)
  , ("first" , fn first)
  , ("rest"  , fn rest)
  , ("seq"   , fn malSeq)
  , ("map"   , fn malMap)
  , ( "conj"
    , fn conj
    )

    -- Map functions
  , ("hash-map" , fn hashMap)
  , ("assoc"    , fn assoc)
  , ("dissoc"   , fn dissoc)
  , ("get"      , fn get)
  , ("contains?", fn containsTest)
  , ("keys"     , fn keys)
  , ( "vals"
    , fn vals
    )

    -- Atom functions
  , ("atom"  , fn atom)
  , ("deref" , fn deref)
  , ("reset!", fn reset)
  , ("swap!" , fn (swap envRef))
  ]

--
-- Basic arithmetic functions
--

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

--
-- Utility functions
--

malThrow :: [AST] -> Mal AST
malThrow [ast] = throwError $ MalError ast
malThrow _     = throwString "Bad arguments for throw"

apply :: [AST] -> Mal AST
apply [ASTFM _ FMFunction _       ] = throwString "No arguments to apply"
apply (ASTFM _ FMFunction f : args) = case last args of
  ASTLV _ _ lastList -> f $ init args ++ lastList
  _                  -> throwString "Expected list as final argument for apply"
apply (ASTFM _ FMMacro _ : _) = throwString "Attempt to apply a macro"
apply _                       = throwString "Bad arguments for apply"

timeMS :: [AST] -> Mal AST
timeMS [] = do
  seconds <- liftIO getPOSIXTime
  let milliSeconds = round $ 1000 * seconds
  return $ ASTInt milliSeconds
timeMS _ = throwString "Expecting no arguments for time-ms"


--
-- IO functions
--

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

malReadline :: [AST] -> Mal AST
malReadline [ASTStr s] = do
  x <- liftIO $ readline (T.unpack s)
  case x of
    Nothing  -> return ASTNil
    Just inp -> return . ASTStr $ T.pack inp
malReadline _ = throwString "readline expects a string"

slurp :: [AST] -> Mal AST
slurp [ASTStr f] = do
  s <- liftIO . TIO.readFile $ T.unpack f
  return $ ASTStr s
slurp _ = throwString "Bad argument type for slurp"

--
-- Sequence functions
--

count :: [AST] -> Mal AST
count [ASTLV _ _ xs] = return (ASTInt (length xs))
count _              = return (ASTInt 0)

cons :: [AST] -> Mal AST
cons [ast, ASTLV _ _ xs] = return $ ASTLV ASTNil LVList (ast : xs)
cons _                   = throwString "Bad arguments for cons"

malConcat :: [AST] -> Mal AST
malConcat (ASTLV _ _ xs : ASTLV _ _ ys : zs) =
  malConcat (ASTLV ASTNil LVList (xs ++ ys) : zs)
malConcat [ASTLV meta _ xs] = return $ ASTLV meta LVList xs
malConcat []                = return $ ASTLV ASTNil LVList []
malConcat _                 = throwString "Bad arguments for concat"

nth :: [AST] -> Mal AST
nth [ASTLV _ _ ys, ASTInt i]
  | i >= 0 && i < length ys = return $ ys !! i
  | otherwise               = throwString "Argument out of bounds in nth"
nth _ = throwString "Bad arguments for nth"

first :: [AST] -> Mal AST
first [ASTLV _ _ (x : _)] = return x
first [ASTLV _ _ []     ] = return ASTNil
first [ASTNil           ] = return ASTNil
first _                   = throwString "Bad arguments for first"

rest :: [AST] -> Mal AST
rest [ASTLV _ _ (_ : ys)] = return $ ASTLV ASTNil LVList ys
rest [ASTLV _ _ []      ] = return $ ASTLV ASTNil LVList []
rest [ASTNil            ] = return $ ASTLV ASTNil LVList []
rest _                    = throwString "Bad arguments for rest"

malSeq :: [AST] -> Mal AST
malSeq [ASTLV _ _ []] = return ASTNil
malSeq [ASTLV _ _ xs] = return $ ASTLV ASTNil LVList xs
malSeq [ASTStr ""   ] = return ASTNil
malSeq [ASTStr s] = return . ASTLV ASTNil LVList . map ASTStr $ T.chunksOf 1 s
malSeq [ASTNil      ] = return ASTNil
malSeq _              = throwString "Unexpected arguments for seq"

malMap :: [AST] -> Mal AST
malMap [ASTFM _ FMFunction f, ASTLV meta _ xs] =
  ASTLV meta LVList <$> mapM (\x -> f [x]) xs
malMap [ASTFM _ FMMacro _, ASTLV{}] = throwString "Attempt to map a macro"
malMap _                            = throwString "Bad arguments for map"

conj :: [AST] -> Mal AST
conj (ASTLV meta LVList xs : args) =
  return $ ASTLV meta LVList (reverse args ++ xs)
conj (ASTLV meta LVVector xs : args) =
  return $ ASTLV meta LVVector (xs ++ args)
conj _ = throwString "Unexpected arguments for conj"

--
-- Hash map functions
--

hashMap :: [AST] -> Mal AST
hashMap xs = do
  xs' <- alternatingToPairs xs
  return $ ASTMap ASTNil (M.fromList xs')

-- Helper function
alternatingToPairs :: [AST] -> Mal [(Text, AST)]
alternatingToPairs (ASTStr a : b : cs) = do
  cs' <- alternatingToPairs cs
  return $ (a, b) : cs'
alternatingToPairs (_ : _ : _) =
  throwString "Expected string or keyword for map associations"
alternatingToPairs [_] =
  throwString "Expected even number of arguments for map associatons"
alternatingToPairs [] = return []

assoc :: [AST] -> Mal AST
assoc (ASTMap meta m : newArgs) = do
  newPairs <- alternatingToPairs newArgs
  let newMap = M.fromList newPairs
  return $ ASTMap meta (M.union newMap m)
assoc _ = throwString "Expected a map"

dissoc :: [AST] -> Mal AST
dissoc (ASTMap meta m : ASTStr x : ys) =
  dissoc (ASTMap meta (M.delete x m) : ys)
dissoc [ASTMap meta m] = return $ ASTMap meta m
dissoc _               = throwString "Bad arguments for dissoc"

get :: [AST] -> Mal AST
get [ASTNil    , ASTStr _] = return ASTNil
get [ASTMap _ m, ASTStr k] = case M.lookup k m of
  Just v  -> return v
  Nothing -> return ASTNil
get _ = throwString "Bad arguments for get"

containsTest :: [AST] -> Mal AST
containsTest [ASTMap _ m, ASTStr k] | M.member k m = return ASTTrue
                                    | otherwise    = return ASTFalse
containsTest _ = throwString "Bad arguments for get"

keys :: [AST] -> Mal AST
keys [ASTMap meta m] = return . ASTLV meta LVList . map ASTStr $ M.keys m
keys _               = throwString "keys expects a hash-map"

vals :: [AST] -> Mal AST
vals [ASTMap meta m] = return . ASTLV meta LVList $ M.elems m
vals _               = throwString "vals expects a hash-map"


--
-- Atom functions
--

atom :: [AST] -> Mal AST
atom [ast] = do
  ref <- liftIO $ newIORef ast
  return $ ASTAtom ref
atom _ = throwString "Bad arguments for atom"

deref :: [AST] -> Mal AST
deref [ASTAtom ref] = liftIO $ readIORef ref
deref _             = throwString "Bad arguments for deref"

reset :: [AST] -> Mal AST
reset [ASTAtom ref, val] = do
  liftIO $ writeIORef ref val
  return val
reset _ = throwString "Bad arguments for reset"

swap :: EnvRef -> [AST] -> Mal AST
swap _ (ASTAtom ref : ASTFM _ FMFunction func : args) = do
  val  <- liftIO $ readIORef ref
  val' <- func (val : args)
  liftIO $ writeIORef ref val'
  return val'
swap _ _ = throwString "Bad arguments for swap"

nyi :: [AST] -> Mal AST
nyi _ = throwString "NYI"

