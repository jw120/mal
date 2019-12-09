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
  ( new
  , empty
  , set
  , get
  , push
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M

import           Types                          ( AST(..)
                                                , Env(..)
                                                , Text
                                                )


-- | Create a new environment with given symbol table
new :: Map Text AST -> Env
new m = Env { envTable = m, envOuter = Nothing }

-- | Empty environment
empty :: Env
empty = Env { envTable = M.empty, envOuter = Nothing }

-- | Takes a symbol key and an AST and adds to the data structure
set :: Text -> AST -> Env -> Env
set sym val e = e { envTable = M.insert sym val (envTable e) }

-- | lookup sumbol ket in the given enviroment
get :: Text -> Env -> Either Text AST
get sym e = case (M.lookup sym (envTable e), envOuter e) of
  (Just a , _         ) -> Right a
  (Nothing, Just outer) -> get sym outer
  (Nothing, Nothing) ->
    Left $ "Symbol '" <> sym <> "' not found in environment"

-- | Set given environment current environment keeping current environemt in outer chain
push :: Env -> Env -> Env
push (Env t Nothing ) current = Env t (Just current)
push (Env t (Just o)) current = Env t (Just (push o current))


