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
  ( empty
  , new
  , replaceTable
  , set
  , get
  , safeGet
  )
where

import           Control.Monad.Except
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

import           Types                          ( AST(..)
                                                , Env(..)
                                                , EnvRef
                                                , Mal(..)
                                                , throwString
                                                , Text
                                                )


-- | Create a brand new, initial environment reference with given symbol table
empty :: Mal EnvRef
empty = liftIO . newIORef $ Env { envTable = M.empty, envOuterRef = Nothing }

-- | Replace the table in the given environment
replaceTable :: EnvRef -> Map Text AST -> Mal ()
replaceTable envRef table = liftIO $ modifyIORef envRef replace'
 where
  replace' :: Env -> Env
  replace' e = e { envTable = table }

-- | Create a new environment with previous environment as its outer chain
new :: EnvRef -> Mal EnvRef
new envRef =
  liftIO . newIORef $ Env { envTable = M.empty, envOuterRef = Just envRef }

-- | Set a value in the environment state
set :: EnvRef -> Text -> AST -> Mal ()
set envRef sym val = liftIO $ modifyIORef' envRef set'
 where
  set' :: Env -> Env
  set' e = e { envTable = M.insert sym val (envTable e) }

-- | lookup a in the enviroment state
get :: EnvRef -> Text -> Mal AST
get envRef sym = do
  maybeVal <- safeGet envRef sym
  case maybeVal of
    Just a  -> return a
    Nothing -> throwString $ "'" <> sym <> "' not found"

-- | lookup a in the enviroment state, returning a Maybe
safeGet :: EnvRef -> Text -> Mal (Maybe AST)
safeGet envRef sym = do
  env <- liftIO $ readIORef envRef
  get' env
 where
  get' :: Env -> Mal (Maybe AST)
  get' e = case (M.lookup sym (envTable e), envOuterRef e) of
    (Just a , _            ) -> return $ Just a
    (Nothing, Just outerRef) -> do
      outer <- liftIO $ readIORef outerRef
      get' outer
    (Nothing, Nothing) -> return Nothing
