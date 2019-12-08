{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- to allow instances for our MalBuiltin type synonym

{-|
Module      : Mal
Description : Convert text to AST
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Defines basic internal data types

-}

module Mal
  ( AST(..)
  , Env(..)
  , Eval
  , MalBuiltin
  , magicKeywordPrefix
  , Text
  )
where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map                       ( Map )
import qualified Data.Map as M
import           Data.Text                      ( Text )

-- | Type for mal functions implemented in Haskell
type MalBuiltin = [AST] -> Either Text AST
instance Show MalBuiltin where
  show _ = "#<function>"
instance Eq MalBuiltin where
  _ == _ = False

type MalClosure = [AST] -> Eval AST
instance Show MalClosure where
  show _ = "#<closure>"
instance Eq MalClosure where
  _ == _ = False

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
  | ASTMap (Map Text AST)
  | ASTBuiltin MalBuiltin
  | ASTClosure MalClosure
  deriving (Eq, Show)

data Env = Env
  { envTable :: Map Text AST -- ^ Symbol table
  , envOuter :: Maybe Env    -- ^ Outer environment for lookup when not in our table
  } deriving (Eq)
instance Show Env where
  show (Env t outer) = show (M.toList (M.filter notBuiltin t)) ++ showOuter outer
    where
      showOuter Nothing = " (no outer)"
      showOuter (Just o) = ", outer: " ++ show o
      notBuiltin (ASTBuiltin _) = False
      notBuiltin _ = True

-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'

-- We use a combined state (for the environment) and error monad
type Eval a = ExceptT Text (State Env) a
