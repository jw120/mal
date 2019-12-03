{-# LANGUAGE OverloadedStrings #-}

module Reader
  ( malRead
  , AST(..)
  , ast
  , Parser
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Bifunctor                 ( first )
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
  runParser   = M.runParser ast "source"

-- | top level-parser for our AST
ast :: Parser AST
ast = spaceConsumer *> (intLiteral <|> stringLiteral)

-- | space consuming parser that eats spaces and comments
spaceConsumer :: Parser ()
spaceConsumer = ML.space MC.space1 (ML.skipLineComment ";") M.empty

-- | Wrapper for lexemes so they consume trailing spaces
lexeme :: Parser a -> Parser a
lexeme = ML.lexeme spaceConsumer

-- | Wrapper for symbols (i.e., verbatim strings) so they consume trailing spaces
symbol :: Text -> Parser Text
symbol = ML.symbol spaceConsumer
-- semicolon = symbol ";"
-- comma     = symbol ","
-- colon     = symbol ":"
-- dot       = symbol "."
tildeAt = symbol "~@"
openSquare = symbol "["
closeSquare = symbol "]"
openCurly = symbol "{"
closeCurly = symbol "}"
singleQuote = symbol "'"
backQuote = symbol "`"
tilde = symbol "~"
hatSign = symbol "^"
atSign = symbol "@"


--parens :: Parser
--parens = between (symbol "(") (symbol ")")

intLiteral :: Parser AST
intLiteral = ASTInt <$> lexeme ML.decimal

stringLiteral :: Parser AST
stringLiteral =
  ASTString
    .   T.pack
    <$> (MC.char '\"' *> M.manyTill ML.charLiteral (MC.char '\"'))


-- [\s,]*: Matches any number of whitespaces or commas. This is not captured so it will be ignored and not tokenized.

-- ~@: Captures the special two-characters ~@ (tokenized).

-- [\[\]{}()'`~^@]: Captures any special single character, one of []{}()'`~^@ (tokenized).

-- "(?:\\.|[^\\"])*"?: Starts capturing at a double-quote and stops at the next double-quote unless it was preceded by a backslash in which case it includes it until the next double-quote (tokenized). It will also match unbalanced strings (no ending double-quote) which should be reported as an error.

-- ;.*: Captures any sequence of characters starting with ; (tokenized).

-- [^\s\[\]{}('"`,;)]*: Captures a sequence of zero or more non special characters (e.g. symbols, numbers, "true", "false", and "nil") and is sort of the inverse of the one above that captures special characters (tokenized).
