module Main where

import Data.Maybe
import Data.Either

import Debug.Trace

import Node.Yargs
import Node.Yargs.Setup
import Node.Yargs.Applicative

import Node.Encoding
import Node.FS.Sync

import Language.TypeScript.Lexer
import Language.TypeScript.Parser
import Language.TypeScript.Docs (generateDocument, htmlToString)

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Control.Monad.Trampoline

import Text.Parsing.Parser

app :: String -> String -> Eff _ Unit
app input output = do
  trace "Reading input file..."
  src <- readTextFile UTF8 input  
  trace "Lexing tokens..."  
  case lex src of
    Left err -> trace $ "Error from lexer: " <> show err
    Right ts -> do
      trace "Parsing TypeScript source..."
      case runTrampoline (runParserT ts declarationSourceFile) of
        Left (ParseError o) -> trace $ "Error from parser: " <> o.message
        Right ds -> do
          let html = htmlToString $ generateDocument ds
          case output of
            "" -> trace html
            _ -> do
              trace "Writing output file..."
              writeTextFile UTF8 output html
          trace "Done!"

main = do
  let setup = usage "$0 -i input-file -o output-file" 
  
  runY setup $ app <$> yarg "i" ["input"]  (Just "The input file")  (Right "The input file is required")  false 
                   <*> yarg "o" ["output"] (Just "The output file") (Left "") false
  
