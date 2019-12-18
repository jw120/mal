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
  , EnvRef
  , Env(..)
  , Mal(..)
  , MalError(..)
  , MalAtom
  , MalFunc
  , magicKeywordPrefix
  , Text
  , astEquality
  , extractInt
  , extractSym
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
  | ASTList [AST]
  | ASTVector [AST]
  | ASTAtom MalAtom
  | ASTMap (Map Text AST)
  | ASTFunc Bool MalFunc
  deriving (Eq, Show)

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
extractInt _          = throwError $ EvalError "Type error: integer expected"

-- | Convert an ASTSymbol to Text (or return an error if not an ASTSym)
extractSym :: AST -> Mal Text
extractSym (ASTSym s) = return s
extractSym _          = throwError $ EvalError "Type error: symbol expected"

-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'

-- Version of equality that treats lists and vectors as the same
astEquality :: AST -> AST -> Bool
astEquality (ASTList   a) (ASTList   b) = astEqualityList a b
astEquality (ASTList   a) (ASTVector b) = astEqualityList a b
astEquality (ASTVector a) (ASTList   b) = astEqualityList a b
astEquality (ASTVector a) (ASTVector b) = astEqualityList a b
astEquality (ASTMap    a) (ASTMap    b) = M.keys a == M.keys b && astEquality
  (ASTList (M.elems a))
  (ASTList (M.elems b))
astEquality x y = x == y
astEqualityList :: [AST] -> [AST] -> Bool
astEqualityList (a : as) (b : bs) =
  a `astEquality` b && as `astEqualityList` bs
astEqualityList [] [] = True
astEqualityList _  _  = False

type EnvRef = IORef Env

data Env = Env
  { envTable :: Map Text AST -- ^ Symbol table
  , envOuter :: Maybe Env    -- ^ Outer environment for lookup when not in our table
  } deriving (Show, Eq)

data MalError
    = ReaderError Text -- ^ Error in Reader module
    | EvalError Text   -- ^ Error in Eval or Core
    | ThrownError AST  -- ^ From malThrow
    deriving (Show)

newtype Mal a = Mal { unMal :: ExceptT MalError (ReaderT Config IO) a }
    deriving (Functor, Applicative, Monad, MonadError MalError, MonadReader Config, MonadIO)

-- | Configuration for the program
newtype Config = Config
  { configDebug :: Bool -- ^ Should we show debug information
  }

-- -- | Exception type used by try*/catch* and throw
-- data MalException = MalException AST deriving (Show)
-- instance Exception MalException