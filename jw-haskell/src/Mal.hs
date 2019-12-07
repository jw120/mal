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
  , MalSpecialLit(..)
  , MalBuiltin
  , magicKeywordPrefix
  , Text
  )
where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

-- | Type for mal functions implemented in Haskell
type MalBuiltin = [AST] -> Either Text AST
instance Show MalBuiltin where
  show _ = "#<function>"
instance Eq MalBuiltin where
  _ == _ = False

-- | Type for our Mal abstract syntax tree
data AST
  = ASTSym Text
  | ASTInt Int
  | ASTStr Text
  | ASTSpecialLit MalSpecialLit
  | ASTList [AST]
  | ASTVector [AST]
  | ASTMap (Map Text AST)
  | ASTBuiltin MalBuiltin
  | ASTClosure Env [AST] AST
  deriving (Eq, Show)

data MalSpecialLit = MalNil | MalTrue | MalFalse deriving (Show, Eq)

data Env = Env
  { envTable :: Map Text AST -- ^ Symbol table
  , envOuter :: Maybe Env    -- ^ Outer environment for lookup when not in our table
  } deriving (Eq, Show)

-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'
