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
  , AST(..)
  , MalSpecialLit(..)
  , MalBuiltin
  , magicKeywordPrefix
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Bifunctor                 ( first )
import           Data.Char                      ( isSpace )
import           Data.Map                       ( Map )
import qualified Data.Map
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as ML

-- | Type for mal functions implemented in Haskell
type MalBuiltin = [AST] -> Either Text AST
instance Show MalBuiltin where
  show _ = "#<function>"
instance Eq MalBuiltin where
  _ == _ = False

data MalSpecialLit = MalNil | MalTrue | MalFalse deriving (Show, Eq)


-- We hold keywords as Strings with a magic prefix
magicKeywordPrefix :: Text
magicKeywordPrefix = "\x029e" -- Unicode 'ʞ'

data AST
  = ASTSym Text
  | ASTInt Int
  | ASTStr Text
  | ASTSpecialLit MalSpecialLit
  | ASTList [AST]
  | ASTVector [AST]
  | ASTMap (Map Text AST)
  | ASTBuiltin MalBuiltin
  deriving (Eq, Show)

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
  [ pIntLiteral
  , M.try pNegIntLiteral  -- try to backtrack if we match the '-'
  , M.try pSpecialLit
  , pStringLiteral
  , pReaderMacro
  , pKeyword
  , pNormalSymbol
  , pList
  , pVector
  , pMap
  ]

-- | Parse an integer literal
pIntLiteral :: Parser AST
pIntLiteral = ASTInt <$> lexeme ML.decimal -- (ML.signed spaceConsumer ML.decimal)

-- | Parse a negative integer literal (a minus sign followed without spaces by a decimal)
pNegIntLiteral :: Parser AST
pNegIntLiteral = ASTInt . (\x -> -x) <$> lexeme (MC.char '-' *> ML.decimal)

-- | Parse a special literial (nil, true or false)
pSpecialLit :: Parser AST
pSpecialLit = M.choice
  [ ASTSpecialLit MalNil <$ symbol "nil"
  , ASTSpecialLit MalTrue <$ symbol "true"
  , ASTSpecialLit MalFalse <$ symbol "false"
  ]

-- | Parse a string literal
pStringLiteral :: Parser AST
pStringLiteral = ASTStr <$> pStringLiteral'
pStringLiteral' :: Parser Text
pStringLiteral' =
  T.pack <$> (MC.char '\"' *> M.manyTill ML.charLiteral (MC.char '\"'))

pReaderMacro :: Parser AST
pReaderMacro = M.choice
  [ (\e -> ASTList [ASTSym "quote", e]) <$> (singleQuote *> pExpr)
  , (\e -> ASTList [ASTSym "quasiquote", e]) <$> (backQuote *> pExpr)
  , (\e -> ASTList [ASTSym "quasiquote", e]) <$> (backQuote *> pExpr)
  , (\e -> ASTList [ASTSym "splice-unquote", e]) <$> (tildeAt *> pExpr)
  , (\e -> ASTList [ASTSym "unquote", e]) <$> (tilde *> pExpr)
  , (\e -> ASTList [ASTSym "deref", e]) <$> (atSign *> pExpr)
  , (\e1 e2 -> ASTList [ASTSym "with-meta", e2, e1])
  <$> (hatSign *> pExpr)
  <*> pExpr
  ]

-- | Parse a normal symbol (a series of non-special characters)
pNormalSymbol :: Parser AST
pNormalSymbol = ASTSym . T.pack <$> lexeme (M.some (M.satisfy isNormal))
  where isNormal c = not (isSpace c) && c `notElem` ("[]{}()'`~^@" :: String)

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

-- -- | Parse a map
-- pMap :: Parser AST
-- pMap = ASTMap . Data.Map.fromList . collectPairs <$> parens (M.many pExpr)
--   where
--     parens = M.between (symbol "{") (symbol "}")
--     collectPairs :: [AST] -> [(Text, AST)]
--     collectPairs [] = []
--     collectPairs [ASTStringLit s] = [(s, ASTSpecialLit MalNil)]
--     collectPairs (ASTStringLit s : y : rest) = (s, y) : collectPairs rest
--     collectPairs _ = error "wrong type for map key"

-- -- | Version of the many combinator that works with a pair of parsers
-- manyPair :: MonadPlus m => m a -> m a -> m [a]
-- manyPair p q = go id
--   where
--     go f = do
--       r <- optional p
--       s <- optional q
--       case (r, s) of
--         (Just x, Just y) -> go (f . (\z -> x:y:z))
--         Nothing -> return (f [])

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
-- semicolon = symbol ";"
-- comma     = symbol ","
-- colon     = symbol ":"
-- dot       = symbol "."
tildeAt :: Parser Text
tildeAt = symbol "~@"
-- openSquare :: Parser Text
-- openSquare = symbol "["
-- closeSquare :: Parser Text
-- closeSquare = symbol "]"
-- openCurly :: Parser Text
-- openCurly = symbol "{"
-- closeCurly :: Parser Text
-- closeCurly = symbol "}"
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






-- [\s,]*: Matches any number of whitespaces or commas. This is not captured so it will be ignored and not tokenized.

-- ~@: Captures the special two-characters ~@ (tokenized).

-- [\[\]{}()'`~^@]: Captures any special single character, one of []{}()'`~^@ (tokenized).

-- "(?:\\.|[^\\"])*"?: Starts capturing at a double-quote and stops at the next double-quote unless it was preceded by a backslash in which case it includes it until the next double-quote (tokenized). It will also match unbalanced strings (no ending double-quote) which should be reported as an error.

-- ;.*: Captures any sequence of characters starting with ; (tokenized).

-- [^\s\[\]{}('"`,;)]*: Captures a sequence of zero or more non special characters (e.g. symbols, numbers, "true", "false", and "nil") and is sort of the inverse of the one above that captures special characters (tokenized).

{-

;; Testing read of quoting
'1
;=>(quote 1)
'(1 2 3)
;=>(quote (1 2 3))
`1
;=>(quasiquote 1)
`(1 2 3)
;=>(quasiquote (1 2 3))
~1
;=>(unquote 1)
~(1 2 3)
;=>(unquote (1 2 3))
`(1 ~a 3)
;=>(quasiquote (1 (unquote a) 3))
~@(1 2 3)
;=>(splice-unquote (1 2 3))

-}
