{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- to allow instances for our MalBuiltin type synonym

{-|
Module      : Reader
Description : Convert text to AST
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Defines our AST data representation and provides the @malRead@ function which lexes and parses
our input text.

Built using megaparsec following the tutorial

https://markkarpov.com/tutorial/megaparsec.html#lexing

-}

module Reader
  ( malRead
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Bifunctor                 ( first )
import           Data.Char                      ( isSpace )
import qualified Data.Map
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as ML

import           Types                          ( AST(..)
                                                , magicKeywordPrefix
                                                )

-- | type for our parsers (void for custom errors, text for the input type)
type Parser = M.Parsec Void Text

-- | Reader function to convert input into an AST
--
-- Returns Left for a parsing failure, Nothing if there is no input (after removing comments/spaces etc)
-- otherwise Right Just the AST
malRead :: Text -> Either Text (Maybe AST)
malRead = first formatError . runParser
 where
  formatError = T.pack . M.errorBundlePretty
  runParser   = M.runParser pTopLevel "source"

-- | top-level parser, catches an empty input (after stripping spaces/comments/commas)
pTopLevel :: Parser (Maybe AST)
pTopLevel = spaceConsumer *> M.choice [Nothing <$ M.eof, Just <$> pExpr]

-- | main parser for our AST
pExpr :: Parser AST
pExpr = spaceConsumer *> M.choice
  [ pIntLiteral -- begins with a digit
  , M.try pNegIntLiteral  -- use try as need to backtrack if we match the '-'
  , pStringLiteral -- begins with a double-quote
  , pReaderMacro -- begins with a special character
  , pKeyword -- begins with a colon
  , pSymbolOrSpecialLit -- begins with a non-special character
  , pList -- begins with a paren
  , pVector -- begings with a square bracket
  , pMap -- begings with a curly bracket
  ]

-- | Parse an integer literal
pIntLiteral :: Parser AST
pIntLiteral = ASTInt <$> lexeme ML.decimal -- (ML.signed spaceConsumer ML.decimal)

-- | Parse a negative integer literal (a minus sign followed without spaces by a decimal)
pNegIntLiteral :: Parser AST
pNegIntLiteral = ASTInt . (\x -> -x) <$> lexeme (MC.char '-' *> ML.decimal)

-- | Parse a string literal
pStringLiteral :: Parser AST
pStringLiteral = ASTStr <$> pStringLiteral'
pStringLiteral' :: Parser Text
pStringLiteral' =
  T.pack <$> (MC.char '\"' *> M.manyTill ML.charLiteral (MC.char '\"'))

pReaderMacro :: Parser AST
pReaderMacro = M.choice
  [ (\e -> ASTList [ASTSym "quote", e]) <$> (symbol "'" *> pExpr)
  , (\e -> ASTList [ASTSym "quasiquote", e]) <$> (symbol "`" *> pExpr)
  , (\e -> ASTList [ASTSym "splice-unquote", e]) <$> (symbol "~@" *> pExpr)
  , (\e -> ASTList [ASTSym "unquote", e]) <$> (symbol "~" *> pExpr)
  , (\e -> ASTList [ASTSym "deref", e]) <$> (symbol "@" *> pExpr)
  , (\e1 e2 -> ASTList [ASTSym "with-meta", e2, e1])
  <$> (symbol "^" *> pExpr)
  <*> pExpr
  ]

-- | Parse a normal symbol (a series of non-special characters) catching special lits
pSymbolOrSpecialLit :: Parser AST
pSymbolOrSpecialLit = toSym <$> lexeme (M.some (M.satisfy isNormal))
  where
    isNormal c = not (isSpace c) && c `notElem` ("[]{}()'`~^@" :: String)
    toSym "nil" = ASTNil
    toSym "true" = ASTTrue
    toSym "false" = ASTFalse
    toSym s = ASTSym $ T.pack s

-- | Parse a keyword (a colon followed by a series of non-special characters)
pKeyword :: Parser AST
pKeyword = ASTStr <$> pKeyword'
pKeyword' :: Parser Text
pKeyword' = toText <$> (MC.char ':' *> lexeme (M.some (M.satisfy isNormal)))
 where
  toText :: String -> Text
  toText = (magicKeywordPrefix <>) . T.pack
  isNormal c = not (isSpace c) && c `notElem` ("[]{}()'`~^@" :: String)

  -- | Parse a list
pList :: Parser AST
pList = ASTList <$> parens (M.many pExpr)
  where parens = M.between (symbol "(") (symbol ")")

-- | Parse a vector
pVector :: Parser AST
pVector = ASTVector <$> parens (M.many pExpr)
  where parens = M.between (symbol "[") (symbol "]")

-- | Parse a map
pMap :: Parser AST
pMap = ASTMap . Data.Map.fromList <$> parens (M.many pMapPair)
 where
  parens = M.between (symbol "{") (symbol "}")
  pMapPair :: Parser (Text, AST)
  pMapPair = (,) <$> (pStringLiteral' <|> pKeyword') <*> pExpr

-- | Helper parser - space consumer that eats spaces, commas and comments
spaceConsumer :: Parser ()
spaceConsumer = ML.space spaceOrComma1 (ML.skipLineComment ";") M.empty
  where spaceOrComma1 = M.skipSome (MC.spaceChar <|> MC.char ',')

-- | Helper parser - wrapper for lexemes so they consume trailing spaces
lexeme :: Parser a -> Parser a
lexeme = ML.lexeme spaceConsumer

-- | Helper parser - wrapper for symbols (i.e., verbatim strings) so they consume trailing spaces
symbol :: Text -> Parser Text
symbol = ML.symbol spaceConsumer
