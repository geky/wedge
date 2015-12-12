module Lex where

import Prelude hiding (lex, repeat)
import Data.Char
import Rule
import Control.Monad


-- Token definitions
data Token
    = Sym   { tsym :: String,   tline :: Line }
    | Term  {                   tline :: Line }
    | Token { tt :: String,     tline :: Line }

instance Show Token where
    show Sym{tsym=sym} = "symbol " ++ show sym
    show Term{}        = "term"
    show Token{tt=tok} = show tok

instance Unexpectable Token where
    line = tline . head


-- Token rules
sym :: Rule Token String
sym = Rule $ \ts -> case ts of
    Sym{tsym=sym}:ts -> Accept sym ts
    ts               -> Reject     ts

term :: Rule Token ()
term = Rule $ \ts -> case ts of
    Term{}:ts -> Accept () ts
    ts        -> Reject    ts

token :: String -> Rule Token ()
token t = Rule $ \ts -> case ts of
    Token{tt=t'}:ts | t == t' -> Accept () ts
    ts                        -> Reject    ts


tokenize :: Rule Char Token
tokenize = Rule $ \cs -> case cs of
    c:cs | elem c "(){}[]"  -> Accept (Token [c] 0) cs
    c:cs | elem c ",;"      -> Accept (Term 0) cs
    '\n':cs                 -> Accept (Term 1) cs
    c:cs | isSpace c        -> step tokenize cs
    c:cs | isAlpha c        -> Accept (Sym sym 0) cs'
      where (sym, cs') = span isAlphaNum (c:cs)
    cs                      -> Reject cs


lex :: String -> [Token]
lex cs = lcount $ run (repeat tokenize) cs
  where lcount = scanl1 (\a b -> b {tline = tline a + tline b})

