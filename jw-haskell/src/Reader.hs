{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Reader
Description : Short description
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
  , AST(..)
  )
where

-- import           Control.Applicative            ( (<|>) )
import           Data.Bifunctor                 ( first )
import           Data.Char                      ( isSpace )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as ML

data AST
  = ASTSymbol Text
  | ASTInt Int
  | ASTString Text
  | ASTList [AST]
  deriving (Eq, Show)

-- | type for our parsers (void for custom errors, text for the input type)
type Parser = M.Parsec Void Text

-- | Reader function to convert input into an AST (or to provide a parsing error)
malRead :: Text -> Either Text AST
malRead = first formatError . runParser
 where
  formatError = T.pack . M.errorBundlePretty
  runParser   = M.runParser pExpr "source"

-- | top level-parser for our AST
pExpr :: Parser AST
pExpr = spaceConsumer *> M.choice
  [ pIntLiteral
  , M.try pNegIntLiteral  -- try to backtrack if we match the '-'
  , pStringLiteral
  , pSpecialSymbol
  , pNormalSymbol
  ]

-- | Parse an integer literal
pIntLiteral :: Parser AST
pIntLiteral = ASTInt <$> lexeme ML.decimal -- (ML.signed spaceConsumer ML.decimal)

-- | Parse a negative integer literal (a minus sign followed without spaces by a decimal)
pNegIntLiteral :: Parser AST
pNegIntLiteral = ASTInt . (\x -> -x) <$> lexeme (MC.char '-' *> ML.decimal)

-- | Parse a string literal
pStringLiteral :: Parser AST
pStringLiteral =
  ASTString
    .   T.pack
    <$> (MC.char '\"' *> M.manyTill ML.charLiteral (MC.char '\"'))

-- | Parse a special symbol (as a single character)
pSpecialSymbol :: Parser AST
pSpecialSymbol = ASTSymbol <$> M.choice
  [ tildeAt
  , openSquare
  , closeSquare
  , openCurly
  , closeCurly
  , singleQuote
  , backQuote
  , tilde
  , hatSign
  , atSign
  ]

-- | Parse a normal symbol (a series of non-special characters)
pNormalSymbol :: Parser AST
pNormalSymbol = ASTSymbol . T.pack <$> lexeme (M.some (M.satisfy isNormal))
  where isNormal c = not (isSpace c) && c `notElem` ("[]{}()'`~^@" :: String)

-- | Helper parser - space consumer that eats spaces and comments
spaceConsumer :: Parser ()
spaceConsumer = ML.space MC.space1 (ML.skipLineComment ";") M.empty

-- | Helper parser - wrapper for lexemes so they consume trailing spaces
lexeme :: Parser a -> Parser a
lexeme = ML.lexeme spaceConsumer

-- | Helper parser - wrapper for symbols (i.e., verbatim strings) so they consume trailing spaces
symbol :: Text -> Parser Text
symbol = ML.symbol spaceConsumer
-- semicolon = symbol ";"
-- comma     = symbol ","
-- colon     = symbol ":"
-- dot       = symbol "."
tildeAt :: Parser Text
tildeAt = symbol "~@"
openSquare :: Parser Text
openSquare = symbol "["
closeSquare :: Parser Text
closeSquare = symbol "]"
openCurly :: Parser Text
openCurly = symbol "{"
closeCurly :: Parser Text
closeCurly = symbol "}"
singleQuote :: Parser Text
singleQuote = symbol "'"
backQuote :: Parser Text
backQuote = symbol "`"
tilde :: Parser Text
tilde = symbol "~"
hatSign :: Parser Text
hatSign = symbol "^"
atSign :: Parser Text
atSign = symbol "@"

--parens :: Parser
--parens = between (symbol "(") (symbol ")")




-- [\s,]*: Matches any number of whitespaces or commas. This is not captured so it will be ignored and not tokenized.

-- ~@: Captures the special two-characters ~@ (tokenized).

-- [\[\]{}()'`~^@]: Captures any special single character, one of []{}()'`~^@ (tokenized).

-- "(?:\\.|[^\\"])*"?: Starts capturing at a double-quote and stops at the next double-quote unless it was preceded by a backslash in which case it includes it until the next double-quote (tokenized). It will also match unbalanced strings (no ending double-quote) which should be reported as an error.

-- ;.*: Captures any sequence of characters starting with ; (tokenized).

-- [^\s\[\]{}('"`,;)]*: Captures a sequence of zero or more non special characters (e.g. symbols, numbers, "true", "false", and "nil") and is sort of the inverse of the one above that captures special characters (tokenized).
