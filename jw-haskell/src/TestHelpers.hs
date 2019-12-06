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
  )
where

import           Data.Text                      ( Text
                                                , isInfixOf
                                                , toLower
                                                )

import           Reader                         ( AST(..) )

i :: Int -> AST
i = ASTInt

isErrorMatching :: Text -> Either Text a -> Bool
isErrorMatching x (Left  t) = toLower x `isInfixOf` toLower t
isErrorMatching _ (Right _) = False

