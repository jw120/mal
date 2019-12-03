{-# LANGUAGE OverloadedStrings #-}

module Printer
  ( malPrint
  , malFormat
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Reader                         ( AST(..) )

-- | Print our AST
malPrint :: Either Text AST -> IO ()
malPrint = TIO.putStrLn . malFormat

malFormat :: Either Text AST -> Text
malFormat (Left  msg) = "Error: " <> msg
malFormat (Right ast) = T.intercalate " " $ go [] ast
 where
  go :: [Text] -> AST -> [Text]
  go acc (ASTSymbol t) = acc ++ [t]
  go acc (ASTInt    i) = acc ++ [T.pack (show i)]
  go acc (ASTString t) = acc ++ ["\"" <> t <> "\""]
  go _   (ASTList   _) = undefined
