{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Env
Description : Environment
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Defines data type for our enviroment (symbol lookup table) and functions
which act on it

-}

module Env
  ( Env -- opaque export
  , emptyWithoutOuter
  , emptyWithOuter
  , get
  , set
  , add
  , new
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )

import           Types                          ( AST()
                                                , Env(..)
                                                )

-- | Create a new environment
new :: Map Text AST -> Env
new m = Env { envTable = m, envOuter = Nothing }

-- | Takes a symbol key and an AST and adds to the data structure
set :: Text -> AST -> Env -> Env
set sym val e = e { envTable = M.insert sym val (envTable e) }

-- | takes a symbol key and uses the find method to locate the environment with the key,
-- then returns the matching value. If no key is found up the outer chain, then throws/raises a "not found" error.
get :: Text -> Env -> Either Text AST
get sym e = case (M.lookup sym (envTable e), envOuter e) of
  (Just a , _         ) -> Right a
  (Nothing, Just outer) -> get sym outer
  (Nothing, Nothing) ->
    Left $ "Symbol '" <> sym <> "' not found in environment"

-- | return a new empty environment with the given outer environment
emptyWithOuter :: Env -> Env
emptyWithOuter outer = Env { envTable = M.empty, envOuter = Just outer }

-- | Empty environment without an outer envirornment
emptyWithoutOuter :: Env
emptyWithoutOuter = Env { envTable = M.empty, envOuter = Nothing }

-- | Set given environment current environment keeping current environemt in outer chain
add :: Env -> Env -> Env
add (Env t Nothing ) current = Env t (Just current)
add (Env t (Just o)) current = Env t (Just (add o current))
