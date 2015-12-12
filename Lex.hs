module Lex where

import Prelude hiding (lex, repeat)
import Data.Char
import Rule
import Control.Applicative
import Numeric


-- Token definitions
data Token
    = Sym    { tsym :: String,      tline :: Line }
    | Int    { tint :: Int,         tline :: Line }
    | Float  { tfloat :: Float,     tline :: Line }
    | String { tstring :: String,   tline :: Line }
    | Term   {                      tline :: Line }
    | Token  { tt :: String,        tline :: Line }

instance Show Token where
    show Sym{tsym=sym}          = "symbol " ++ show sym
    show Int{tint=int}          = "int " ++ show int
    show Float{tfloat=float}    = "float " ++ show float
    show String{tstring=string} = "string " ++ show string
    show Term{}                 = "term"
    show Token{tt=tok}          = show tok

instance Unexpectable Token where
    line = tline . head


-- Token matching rules
sym :: Rule Token String
sym = Rule $ \ts -> case ts of
    Sym{tsym=sym}:ts -> Accept sym ts
    ts               -> Reject ts

int :: Rule Token Int
int = Rule $ \ts -> case ts of
    Int{tint=int}:ts -> Accept int ts
    ts               -> Reject ts

float :: Rule Token Float
float = Rule $ \ts -> case ts of
    Float{tfloat=float}:ts -> Accept float ts
    ts                     -> Reject ts

string :: Rule Token String
string = Rule $ \ts -> case ts of
    String{tstring=string}:ts -> Accept string ts
    ts                        -> Reject ts

term :: Rule Token ()
term = Rule $ \ts -> case ts of
    Term{}:ts -> Accept () ts
    ts        -> Reject    ts

token :: String -> Rule Token ()
token t = Rule $ \ts -> case ts of
    Token{tt=t'}:ts | t == t' -> Accept () ts
    ts                        -> Reject ts


-- Tokenizing rules
tokSym :: Rule Char String
tokSym = Rule $ \cs -> case span isAlphaNum cs of
    ("",  cs) -> Reject cs
    (sym, cs) -> Accept sym cs

tokNum :: Int -> Rule Char Int
tokNum base = Rule $ \cs -> case span valid cs of
    ("", cs) -> Reject cs
    (ds, cs) -> Accept (foldr1 acc $ map digitToInt ds) cs
  where
    valid a = isDigit a && digitToInt a < base
    acc a b = a + b*base

tokInt :: Rule Char Int
tokInt = Rule $ \cs -> case cs of
    '0':b:cs | elem b "bB" -> tokNum 2  `step` cs
    '0':b:cs | elem b "oO" -> tokNum 8  `step` cs
    '0':b:cs | elem b "xX" -> tokNum 16 `step` cs
    cs                     -> tokNum 10 `step` cs

tokenize :: Rule Char Token
tokenize = Rule $ \cs -> case cs of
    c:cs     | elem c "(){}[]" -> Accept (Token [c] 0) cs
    c:cs     | elem c ",;"     -> Accept (Term 0) cs
    '\n':cs                    -> Accept (Term 1) cs
    c:cs     | isSpace c       -> step tokenize cs
    cs@(c:_) | isAlpha c       -> flip Sym 0 <$> tokSym `step` cs
    cs@(c:_) | isDigit c       -> flip Int 0 <$> tokInt `step` cs
    cs                         -> Reject cs


lex :: String -> [Token]
lex cs = lcount $ run (repeat tokenize) cs
  where lcount = scanl1 (\a b -> b {tline = tline a + tline b})

