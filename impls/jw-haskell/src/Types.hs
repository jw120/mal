{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- to allow instances for our MalBuiltin type synonym
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- to simplify use of our monad stack

{-|
Module      : Mal
Description : Data type definitions
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Defines basic internal data types

-}

module Types
  ( AST(..)
  , Metadata(..)
  , noMeta
  , FMType(..)
  , LVType(..)
  , EnvRef
  , Env(..)
  , Mal(..)
  , MalError(..)
  , throwString
  , MalAtom
  , MalFunc
  , magicKeywordPrefix
  , Text
  , collectionEquality
  , extractInt
  , extractSym
  , boolToAST
  , Config(..)
--   , MalException(..)
  )
where

-- import Control.Exception (Exception)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )


-- | Type for our Mal abstract syntax tree
data AST
  = ASTNil
  | ASTTrue
  | ASTFalse
  | ASTSym Text
  | ASTInt Int
  | ASTStr Text
    -- Composite data types and functions have a metadata attribute
  | ASTFM Metadata FMType MalFunc -- Function or Macro
  | ASTLV Metadata LVType [AST] -- List or Vector
  | ASTMap Metadata (Map Text AST)
  | ASTAtom Metadata MalAtom
  deriving (Eq, Show)

newtype Metadata = Metadata AST deriving (Eq, Show)

-- Abbreviation
noMeta :: Metadata
noMeta = Metadata ASTNil

data LVType = LVList | LVVector deriving (Eq, Show)
data FMType = FMFunction | FMMacro deriving (Eq, Show)

-- | Type for mal functions
type MalFunc = [AST] -> Mal AST
instance Show MalFunc where
  show _ = "#<function>"
instance Eq MalFunc where
  _ == _ = False

-- | Type for atoms
type MalAtom = IORef AST
instance Show MalAtom where
  show _ = "#<atom>"

-- | Convert an ASTInt to Int (or return an error if not an ASTInt)
extractInt :: AST -> Mal Int
extractInt (ASTInt i) = return i
extractInt _          = throwString "Type error: integer expected"

-- | Convert an ASTSymbol to Text (or return an error if not an ASTSym)
extractSym :: AST -> Mal Text
extractSym (ASTSym s) = return s
extractSym _          = throwString "Type error: symbol expected"

-- Convert a Boolean to the correpsonding AST type
boolToAST :: Bool -> AST
boolToAST True  = ASTTrue
boolToAST False = ASTFalse

-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'

-- Version of equality that treats lists and vectors as the same
collectionEquality :: AST -> AST -> Bool
collectionEquality (ASTLV _ _ xs) (ASTLV _ _ ys) = listsEqual xs ys
collectionEquality (ASTMap _ m1) (ASTMap _ m2) =
  M.keys m1 == M.keys m2 && listsEqual (M.elems m1) (M.elems m2)
collectionEquality x y = x == y

-- Helper function for collectionEquality
listsEqual :: [AST] -> [AST] -> Bool
listsEqual (a : as) (b : bs) = a `collectionEquality` b && as `listsEqual` bs
listsEqual []       []       = True
listsEqual _        _        = False

type EnvRef = IORef Env

data Env = Env
  { envTable :: Map Text AST -- ^ Symbol table
  , envOuter :: Maybe Env    -- ^ Outer environment for lookup when not in our table
  } deriving (Show, Eq)

-- | Errors/exceptions caught in an ExceptT (=Either) monad
newtype MalError = MalError AST

-- | Helper fundtion to throw a string
throwString :: Text -> Mal a
throwString = throwError . MalError . ASTStr

newtype Mal a = Mal { unMal :: ExceptT MalError (ReaderT Config IO) a }
    deriving (Functor, Applicative, Monad, MonadError MalError, MonadReader Config, MonadIO)

-- | Configuration for the program
newtype Config = Config
  { configDebug :: Bool -- ^ Should we show debug information
  }
