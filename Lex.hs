module Lex where

import Prelude hiding (lex)
import Data.Char
import Rule
import Control.Applicative
import Numeric


-- Token definitions
data Token
    = Sym    { tsym :: String,      tline :: Line }
    | Int    { tint :: Int,         tline :: Line }
    | Float  { tfloat :: Double,    tline :: Line }
    | String { tstring :: String,   tline :: Line }
    | Sep    {                      tline :: Line }
    | Term   {                      tline :: Line }
    | Token  { tt :: String,        tline :: Line }

instance Show Token where
    show Sym{tsym=sym}          = "symbol " ++ show sym
    show Int{tint=int}          = "int " ++ show int
    show Float{tfloat=float}    = "float " ++ show float
    show String{tstring=string} = "string " ++ show string
    show Sep{}                  = "sep"
    show Term{}                 = "term"
    show Token{tt=tok}          = show tok

instance Eq Token where
    Sym{tsym=a}       == Sym{tsym=b}       = a == b
    Int{tint=a}       == Int{tint=b}       = a == b
    Float{tfloat=a}   == Float{tfloat=b}   = a == b
    String{tstring=a} == String{tstring=b} = a == b
    Sep{}             == Sep{}             = True
    Term{}            == Term{}            = True
    Token{tt=a}       == Token{tt=b}       = True
    _                 == _                 = False

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

string :: Rule Token String
string = Rule $ \ts -> case ts of
    String{tstring=string}:ts -> Accept string ts
    ts                        -> Reject ts

sep :: Rule Token ()
sep = Rule $ \ts -> case ts of
    Sep{}:ts -> Accept () ts
    ts       -> Reject ts

term :: Rule Token ()
term = Rule $ \ts -> case ts of
    Term{}:ts -> Accept () ts
    ts        -> Reject ts

token :: String -> Rule Token ()
token t = Rule $ \ts -> case ts of
    Token{tt=t'}:ts | t == t' -> Accept () ts
    ts                        -> Reject ts


-- Tokenizing rules
tokSym :: Rule Char (Line -> Token)
tokSym = Rule $ \cs -> case span isAlphaNum cs of
    ("",  cs) -> Reject cs
    (sym, cs) -> Accept (Sym sym) cs

tokNum :: Int -> Rule Char Int
tokNum base = Rule $ \cs -> case span valid cs of
    ("", cs) -> Reject cs
    (ds, cs) -> Accept (foldr1 acc $ map digitToInt ds) cs
  where
    valid a = isDigit a && digitToInt a < base
    acc a b = a + b*base

tokFrac :: Double -> Rule Char Double
tokFrac base = Rule $ \cs -> case span valid cs of
    ("", cs) -> Reject cs
    (ds, cs) -> Accept ((/base) $ foldr1 acc $ map (fromIntegral . digitToInt) ds) cs
  where
    valid a = isDigit a && fromIntegral (digitToInt a) < base
    acc a b = a + b/base

tokInt :: Rule Char (Line -> Token)
tokInt = Rule $ \cs -> case cs of
    '0':b:cs | elem b "bB" -> step (Int <$> tokNum 2)  cs
    '0':b:cs | elem b "oO" -> step (Int <$> tokNum 8)  cs
    '0':b:cs | elem b "xX" -> step (Int <$> tokNum 16) cs
    cs                     -> step (Int <$> tokNum 10) cs

tokFloat :: Rule Char (Line -> Token)
tokFloat = Rule $ \cs -> case cs of
    cs -> step ((\a b -> Float (fromIntegral a + b)) <$> tokNum 10 <* match '.' <*> tokFrac 10) cs
--(Float . (+) . fromIntegral) <$> tokNum 10 <* match '.' <*> tokFrac 10

tokenize :: Rule Char (Line -> Token)
tokenize = Rule $ \cs -> case cs of
    '-':'>':cs                 -> Accept (Token "->") cs
    c:cs     | elem c "(){}[]" -> Accept (Token [c]) cs
    ',':cs                     -> Accept Sep cs
    c:cs     | elem c ";\n"    -> Accept Term cs
    c:cs     | isSpace c       -> step tokenize cs
    cs@(c:_) | isAlpha c       -> step tokSym cs
    cs@(c:_) | isDigit c       -> step (tokFloat <|> tokInt) cs
    cs                         -> Reject cs

toklines :: Rule Char Line
toklines = Rule $ \cs -> case cs of
    '\n':cs -> Accept 1 cs
    cs      -> step (0 <$ tokenize) cs

lex :: String -> [Token]
lex cs = zipWith ($) tokens lines
  where
    tokens = run (many tokenize) cs
    lines = scanl1 (+) $ run (many toklines) cs

