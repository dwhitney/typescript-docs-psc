module Language.TypeScript.Lexer where

import Data.Char
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Either
import Data.Array.ST
import Data.Function
import Data.String (length, charAt, take, drop)  
import Data.String.Regex
import Data.Foldable (elem, foldl)
    
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.ST    
import Control.Monad.Rec.Class    
    
data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LAngle
  | RAngle
  | LSquare
  | RSquare
  | Arrow
  | Colon
  | Equals
  | Dot
  | Comma
  | Semi
  | QuestionMark
  | Ellipsis
  | IdentOrKeyword String
  | Natural Number
  | StringLiteral String
  | LineComment String
  | BlockComment String
  
instance showToken :: Show Token where
  show LParen             = "LParen"   
  show RParen             = "RParen"   
  show LBrace             = "LBrace"   
  show RBrace             = "RBrace"   
  show LAngle             = "LAngle"   
  show RAngle             = "RAngle"   
  show LSquare            = "LSquare"  
  show RSquare            = "RSquare"  
  show Arrow              = "Arrow"    
  show Colon              = "Colon"    
  show Equals             = "Equals"   
  show Dot                = "Dot"      
  show Comma              = "Comma"    
  show Semi               = "Semi"   
  show QuestionMark       = "QuestionMark"
  show Ellipsis           = "Ellipsis"  
  show (IdentOrKeyword s) = "(IdentOrKeyword " ++ show s ++ ")"
  show (Natural n)        = "(Natural " ++ show n ++ ")"
  show (StringLiteral s)  = "(StringLiteral " ++ show s ++ ")"
  show (LineComment s)    = "(LineComment " ++ show s ++ ")"
  show (BlockComment s)   = "(BlockComment " ++ show s ++ ")"
  
instance eqToken :: Eq Token where
  (==) LParen              LParen              = true
  (==) RParen              RParen              = true
  (==) LBrace              LBrace              = true
  (==) RBrace              RBrace              = true
  (==) LAngle              LAngle              = true
  (==) RAngle              RAngle              = true
  (==) LSquare             LSquare             = true
  (==) RSquare             RSquare             = true
  (==) Arrow               Arrow               = true
  (==) Colon               Colon               = true
  (==) Equals              Equals              = true
  (==) Dot                 Dot                 = true
  (==) Comma               Comma               = true
  (==) Semi                Semi                = true
  (==) QuestionMark        QuestionMark        = true
  (==) Ellipsis            Ellipsis            = true
  (==) (IdentOrKeyword s1) (IdentOrKeyword s2) = s1 == s2
  (==) (Natural n1)        (Natural n2)        = n1 == n2
  (==) (StringLiteral s1)  (StringLiteral s2)  = s1 == s2
  (==) (LineComment s1)    (LineComment s2)    = s1 == s2
  (==) (BlockComment s1)   (BlockComment s2)   = s1 == s2
  (==) _                   _                   = false
  (/=) t1                  t2                  = not (t1 == t2)
  
type PosToken = 
  { token  :: Token
  , line   :: Number
  , column :: Number
  , comments :: [String]
  }
  
foreign import parseString
  "function parseString(s) {\
  \  return JSON.parse(s);\
  \}" :: forall a. String -> a
  
type LexerState = { index :: Number, line :: Number, column :: Number }  
  
lex :: String -> Either Error [PosToken]
lex s = runPure (catchException (return <<< Left) (Right <<< removeComments <$> runSTArray (do
  ts <- emptySTArray
  build ts
  return ts)))
  where
  build :: forall h eff. STArray h PosToken -> Eff (st :: ST h, err :: Exception | eff) Unit
  build ts = tailRecM go { index: 0, line: 1, column: 1 }
    where
    go :: forall eff. LexerState -> Eff (st :: ST _, err :: Exception | eff) (Either LexerState Unit)
    go { index: i, line: l, column: c } 
      | i >= length s = return $ Right unit
      | isNewLine    (charAt i s) = return $ Left { index: i + 1, line: l + 1, column: 1     }
      | isWhitespace (charAt i s) = return $ Left { index: i + 1, line: l    , column: c + 1 }
      | otherwise = 
        case charString (fromJust (charAt i s)) of
          "(" -> emit LParen  
          ")" -> emit RParen  
          "{" -> emit LBrace  
          "}" -> emit RBrace  
          "<" -> emit LAngle  
          ">" -> emit RAngle  
          "[" -> emit LSquare 
          "]" -> emit RSquare 
          ":" -> emit Colon   
          "," -> emit Comma   
          ";" -> emit Semi    
          "?" -> emit QuestionMark
          "." -> case take 2 $ drop (i + 1) s of
                   ".." -> emitThen Ellipsis (i + 3) (c + 3)
                   _    -> emit Dot
          "=" -> case charString <$> charAt (i + 1) s of
                   Just ">" -> emitThen Arrow (i + 2) (c + 2)
                   _        -> emit Equals
          "/" -> case charString <$> charAt (i + 1) s of
                   Just "/" -> readLineComment (i + 2)
                   Just "*" -> readBlockComment (i + 2) l c
                   _        -> throwException $ error $ "Expected comment at line " ++ show l ++ ", column " ++ show c
          _  | isStringLiteral i -> readStringLiteral i c
             | isNumericLiteral i -> readNumericLiteral i c
          ch | isIdentStart ch -> readIdent i c
             | otherwise -> throwException $ error $ "Unexpected character " ++ show ch ++ " at line " ++ show l ++ ", column " ++ show c
      where
      emit :: Token -> Eff _ (Either LexerState Unit)      
      emit tok = emitThen tok (i + 1) (c + 1)
    
      emitThen :: Token -> Number -> Number -> Eff _ (Either LexerState Unit)
      emitThen tok next col = emitThen' tok next l col
    
      emitThen' :: Token -> Number -> Number -> Number -> Eff _ (Either LexerState Unit)
      emitThen' tok next line col = do
        ts `pushSTArray` { token: tok, line: line, column: col, comments: [] }
        return $ Left { index: next, line: l, column: col }
          
      readLineComment :: Number -> Eff _ (Either LexerState Unit)
      readLineComment i = collect 0
        where 
        collect len = 
          case strAt (i + len) of
            Just "\r" -> lineComment i len
            Just "\n" -> lineComment i len
            Nothing   -> lineComment i len
            _         -> collect (len + 1)
      
        lineComment i len = emitThen' (LineComment (take len (drop i s))) (i + len + 1) 1 (l + 1)
          
      readBlockComment :: Number -> Number -> Number -> Eff _ (Either LexerState Unit)      
      readBlockComment i l c = collect 0 l c
        where 
        collect len l c = 
          case two <$> strAt (i + len) <*> strAt (i + len + 1) of
            Just ["*", "/"] -> blockComment l c len
            Just ["\n", _]  -> collect (len + 1) (l + 1) 1
            Just _          -> collect (len + 1) l       (c + 1)
            Nothing         -> throwException $ error "Unterminated block comment"
      
        two x y = [x, y]
      
        blockComment l c len = emitThen' (BlockComment (take len (drop i s))) (i + len + 2) l (c + 2)
    
      stringLiteralRegex :: Regex
      stringLiteralRegex = regex "^\"(?:\\\\.|[^\"])*\"" flags
        where
        flags = { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false }
    
      isStringLiteral :: Number -> Boolean
      isStringLiteral i = test stringLiteralRegex (drop i s) 
    
      readStringLiteral :: Number -> Number -> Eff _ (Either LexerState Unit)
      readStringLiteral i c = 
        case match stringLiteralRegex (drop i s) of
          Just (s : _) -> emitThen (StringLiteral (parseString s)) (i + length s) (c + length s)
          
      numericLiteralRegex :: Regex
      numericLiteralRegex = regex "^-?[0-9]+" flags
        where
        flags = { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false }      
          
      isNumericLiteral :: Number -> Boolean
      isNumericLiteral i = test numericLiteralRegex (drop i s) 
    
      readNumericLiteral :: Number -> Number -> Eff _ (Either LexerState Unit)
      readNumericLiteral i c = 
        case match numericLiteralRegex (drop i s) of
          Just (s : _) -> emitThen (Natural (parseString s)) (i + length s) (c + length s)
            
      readIdent :: Number -> Number -> Eff _ (Either LexerState Unit)         
      readIdent i col = collect i col ""
        where
        collect j col acc = 
          case strAt j of
            Just c | isIdentChar c -> collect (j + 1) (col + 1) (acc <> c)
            _ -> emitThen (IdentOrKeyword acc) j col
    
  removeComments :: [PosToken] -> [PosToken]
  removeComments ts = (foldl (\st t -> 
    case t.token of
      LineComment s -> { comments: st.comments <> [s], tokens: st.tokens }
      BlockComment s -> { comments: st.comments <> [s], tokens: st.tokens }
      _ -> { comments: [], tokens: st.tokens <> [t { comments = st.comments }] }) { comments: [], tokens: [] } ts).tokens
    
  strAt :: Number -> Maybe String
  strAt i = charString <$> charAt i s    
    
  toDigit :: String -> Maybe Number
  toDigit "0" = Just 0
  toDigit "1" = Just 1
  toDigit "2" = Just 2
  toDigit "3" = Just 3
  toDigit "4" = Just 4
  toDigit "5" = Just 5
  toDigit "6" = Just 6
  toDigit "7" = Just 7
  toDigit "8" = Just 8
  toDigit "9" = Just 9
  toDigit _   = Nothing
  
  isLower :: String -> Boolean
  isLower c = c >= "a" && c <= "z"

  isUpper :: String -> Boolean
  isUpper c = c >= "A" && c <= "Z"

  isNumeric :: String -> Boolean
  isNumeric c = c >= "0" && c <= "9"

  isAlpha :: String -> Boolean
  isAlpha c = isLower c || isUpper c
  
  isAlphaNum :: String -> Boolean
  isAlphaNum c = isAlpha c || isNumeric c
  
  isIdentStart :: String -> Boolean
  isIdentStart c = isLower c || isUpper c || (c == "_") || (c == "$")
  
  isIdentChar :: String -> Boolean
  isIdentChar c = isIdentStart c || isNumeric c
    
  isWhitespace :: Maybe Char -> Boolean
  isWhitespace (Just c) = 
    case charString c of
      " "  -> true
      "\r" -> true
      "\n" -> true
      "\t" -> true
      _    -> false
  isWhitespace Nothing = false    

  isNewLine :: Maybe Char -> Boolean
  isNewLine (Just c) = charString c == "\n" 
  isNewLine Nothing = false   