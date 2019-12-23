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
  , throwString
  , MalAtom
  , MalFunc
  , magicKeywordPrefix
  , Text
  , astEquality
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
  | ASTAtom MalAtom
    -- Composite data types and functions have a metadata attribute
  | ASTList [AST] AST
  | ASTVector [AST] AST
  | ASTMap (Map Text AST) AST
  | ASTFunc Bool MalFunc AST -- Function or (if bool is true) macro
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
extractInt _          = throwString "Type error: integer expected"

-- | Convert an ASTSymbol to Text (or return an error if not an ASTSym)
extractSym :: AST -> Mal Text
extractSym (ASTSym s) = return s
extractSym _          = throwString "Type error: symbol expected"

-- Convert a Boolean to the correpsonding AST type
boolToAST :: Bool -> AST
boolToAST True = ASTTrue
boolToAST False = ASTFalse

-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'

-- Version of equality that treats lists and vectors as the same
astEquality :: AST -> AST -> Bool
astEquality (ASTList   a _) (ASTList   b _) = astEqualityList a b
astEquality (ASTList   a _) (ASTVector b _) = astEqualityList a b
astEquality (ASTVector a _) (ASTList   b _) = astEqualityList a b
astEquality (ASTVector a _) (ASTVector b _) = astEqualityList a b
astEquality (ASTMap    a _) (ASTMap    b _) = M.keys a == M.keys b && astEquality
  (ASTList (M.elems a) ASTNil)
  (ASTList (M.elems b) ASTNil)
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
