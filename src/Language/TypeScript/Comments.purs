module Language.TypeScript.Comments (ParsedComment(), parseComment) where

import Data.Tuple
import Data.Maybe (fromMaybe)
import Data.Array (head, map, drop, concatMap)

import qualified Data.String as S
import qualified Data.String.Regex as R    
    
import Language.TypeScript.Types

type ParsedComment =
  { text     :: [String]
  , other    :: [Tuple String String]
  }

getContent :: String -> [String]
getContent = map dropStar <<< lines
  where
    dropStar :: String -> String
    dropStar = R.replace star ""
    
    star :: R.Regex
    star = R.regex "^(\\s)*[*](\\s)*" { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false }
    
    lines :: String -> [String]
    lines = S.split "\n"

parseComment :: Comment -> ParsedComment
parseComment (Comment ss) = go [] [] true $ concatMap getContent ss
  where
  go :: [String] -> [Tuple String String] -> Boolean -> [String] -> ParsedComment
  go text other _  [] = { text: text, other: other }
  go text other lf (s : ss) 
    | S.take 1 s == "@" =
      let words = S.split " " (S.drop 1 s)
          key = fromMaybe "" (head words)
          value = S.joinWith " " (drop 1 words)
      in go text (other ++ [Tuple key (S.trim value)]) true ss
    | isSpace s = 
      go text other true ss
    | lf =
      go (text ++ [s]) other false ss
    | otherwise = 
      go (append s text) other false ss

  isSpace :: String -> Boolean
  isSpace = R.test space
  
  space :: R.Regex
  space = R.regex "^(\\s)*$" { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false }

  append :: String -> [String] -> [String]
  append s [] = [s]
  append s [s1] = [s1 ++ " " ++ s]
  append s (s1:ss) = s1 : append s ss
  