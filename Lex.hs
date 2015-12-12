module Lex where

import Prelude hiding (lex, repeat)
import Data.Char
import Rule
import Control.Monad


-- Line numbers in file
type Line = Int


-- Tokens and related rules
data Token
    = Sym   { sym :: String,    line :: Line }
    | Term  {                   line :: Line }
    | Token { tok :: String,    line :: Line }
    deriving Show

tsym :: Rule Token String
tsym = Rule $ \ts -> case ts of
    Sym{sym=sym}:ts -> Accept sym ts
    ts              -> Reject     ts

tterm :: Rule Token ()
tterm = Rule $ \ts -> case ts of
    Term{}:ts -> Accept () ts
    ts        -> Reject    ts

token :: String -> Rule Token ()
token t = Rule $ \ts -> case ts of
    Token{tok=t'}:ts | t == t' -> Accept () ts
    ts                         -> Reject    ts


tokenize :: Rule Char Token
tokenize = Rule $ \cs -> case cs of
    c:cs | elem c "(){}[]"  -> Accept (Token [c] 0) cs
    c:cs | elem c ",;"      -> Accept (Term 0) cs
    '\n':cs                 -> Accept (Term 1) cs
    c:cs | isSpace c        -> step tokenize cs
    c:cs | isAlpha c        -> Accept (Sym sym 0) cs'
      where (sym, cs') = span isAlphaNum (c:cs)
    cs                      -> Reject cs

unexpected :: [Token] -> a
unexpected ts = error $ "unexpected " ++ case ts of
    Sym{sym=sym, line=line}:ts -> "symbol "++show sym++" on line "++show line
    Token{tok=t, line=line}:ts -> "token "++show t++" on line "++show line
    Term{line=line}:ts         -> "terminator on line "++show line
    _                          -> "end of input"


lex :: String -> [Token]
lex cs = lcount $ run unexpected (repeat tokenize) cs
  where
    lcount = map (\a -> a {line = line a + 1})
           . scanl1 (\a b -> b {line = line a + line b})

    unexpected cs = error $ "unexpected " ++ case cs of
        cs'@(c:_) -> show c ++ " on line " ++ show n
          where n = length $ filter (=='\n') $ take (length cs - length cs') cs
        _         -> "end of input"

