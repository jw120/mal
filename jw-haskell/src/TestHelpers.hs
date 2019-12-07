{-|
Module      : TestHelpers
Description : Helper functions for testing
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Helper functions for testing

-}

module TestHelpers
  ( i
  , isErrorMatching
  , kw
  , kwText
  , list
  , m
  , s
  , sym
  , vec
  )
where

import qualified Data.Map                      as M
import           Data.Text                      ( Text
                                                , isInfixOf
                                                , toLower
                                                )

import           Mal                            ( AST(..)
                                                , magicKeywordPrefix
                                                )

i :: Int -> AST
i = ASTInt

kw :: Text -> AST
kw = ASTStr . kwText

kwText :: Text -> Text
kwText = (magicKeywordPrefix <>)

list :: [AST] -> AST
list = ASTList

m :: [(Text, AST)] -> AST
m = ASTMap . M.fromList

s :: Text -> AST
s = ASTStr

sym :: Text -> AST
sym = ASTSym

vec :: [AST] -> AST
vec = ASTVector

isErrorMatching :: Text -> Either Text a -> Bool
isErrorMatching x (Left  t) = toLower x `isInfixOf` toLower t
isErrorMatching _ (Right _) = False
