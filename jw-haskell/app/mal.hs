{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : mal
Description : Main program module for mal interpreter
Copyright   : (c) Joe Watson, 2019
License     : GPL-3
Maintainer  : joe_watson@mail.com

Main module and drivers of repl.

-}

module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import Data.Semigroup ((<>)) -- For Options.Applicative
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import Options.Applicative
import           System.Console.Readline        ( readline
                                                , addHistory
                                                )

import qualified Core
import qualified Env
import           Eval                           ( eval )
import           Types                          ( Mal(..)
                                                , EnvRef
                                                , AST(..)
                                                , Text
                                                , Config(..)
                                                )
import           Printer                        ( malPrint )
import           Reader                         ( malRead )


data Input =
    FileInput FilePath [Text]
    | ExprInput Text

exprInput :: Parser Input
exprInput = ExprInput <$> strOption
    (  long "expr"
    <> short 'e'
    <> metavar "EXPR"
    <> help "Expression to evaluate" )

fileInput :: Parser Input
fileInput = FileInput
    <$> strArgument
       ( metavar "FILENAME"
       <> help "Input file" )
    <*> some
        ( argument str (metavar "ARGUMENTS..."
        <> help "Arguments to pass to the interpeter"))

-- replInput :: Parser Input
-- replInput = flag' ReplInput
--     (  long "repl"
--     <> help "Start repl" )

input :: Parser Input
input = exprInput <|> fileInput

data Options = Options
    { optDebug :: Bool
    , optInput :: Maybe Input
    }

options :: Parser Options
options = Options
    <$> switch
        ( long "debug"
        <> short 'd'
        <> help "Enable debug information mode" )
    <*> optional input

parseOptions :: ParserInfo Options
parseOptions = info (options <**> helper)
           ( fullDesc
              <> progDesc "Evaluate the expression, load the file with given arv, or start the repl"
              <> header "mal - a make-a-lisp interpreter" )

main :: IO ()
main = do
    opts <- execParser parseOptions
    let malMain' = unMal $ malMain (optInput opts)
    let readerConfig = Config { configDebug = optDebug opts }
    void $ runReaderT (runExceptT malMain') $ readerConfig

malMain :: Maybe Input -> Mal ()
malMain inp = do
  envRef <- Env.empty
  Env.replaceTable envRef (Core.nameSpace envRef)
  mapM_ (rep envRef True) Core.prelude
  Env.set envRef "*ARGV*" $ ASTList []
  case inp of
    Nothing -> repl envRef
    Just (ExprInput e) -> rep envRef False e
    Just (FileInput fn argv) -> do
        Env.set envRef "*ARGV*" $ ASTList (map ASTStr argv)
        rep envRef True $ T.pack ("(load-file \"" ++ fn ++ "\")")

-- Read-evaluate-print
rep :: EnvRef -> Bool -> Text -> Mal ()
rep envRef quiet src = case malRead src of
  Left  readError  -> liftIO $ TIO.putStrLn ("Read error: " <> readError)
  Right Nothing    -> return ()
  Right (Just ast) -> do
    val <- eval envRef ast `catchError` (\e -> return (ASTStr ("Error: " <> e)))
    if quiet then return () else malPrint val

-- repl - iterate rep repeatedly
repl :: EnvRef -> Mal ()
repl envRef = do
  x <- liftIO $ readline "mal> "
  case x of
    Nothing   -> return ()
    Just line -> do
      liftIO $ addHistory line
      rep envRef False $ T.pack line
      repl envRef

