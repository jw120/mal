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
  , get
  , new
  , set
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )

import           Reader                         ( AST(..) )

data Env = Env
  { envTable :: Map Text AST -- ^ Symbol table
  , envOuter :: Maybe Env    -- ^ Outer environment for lookup when not in our table
  }

-- | Takes a symbol key and an AST and adds to the data structure
set :: Env -> (Text, AST) -> Env
set = undefined

-- | takes a symbol key and uses the find method to locate the environment with the key,
-- then returns the matching value. If no key is found up the outer chain, then throws/raises a "not found" error.
get :: Env -> Text -> Either Text AST
get = undefined

-- | return a new empty environment with the given parent
new :: Maybe Env -> Env
new outer = Env { envTable = M.empty, envOuter = outer }

  -- | Takes a symbol key and if the current environment contains that key then return the environment.
-- If no key is found and outer is not nil then call find (recurse) on the outer environment.
--find :: Env -> AST -> Maybe Env
--find = undefined



