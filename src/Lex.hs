module Lex where

import Prelude hiding (lex)
import Data.Char
import Rule
import Control.Applicative


-- Token definitions
data Token
    = Sym    { tsym :: String,      tline :: Line }
    | Int    { tint :: Int,         tline :: Line }
    | Float  { tfloat :: Double,    tline :: Line }
    | String { tstring :: String,   tline :: Line }
    | Term   {                      tline :: Line }
    | Token  { ttoken :: String,    tline :: Line }

instance Show Token where
    show Sym{tsym=s}       = "symbol " ++ show s
    show Int{tint=i}       = "int " ++ show i
    show Float{tfloat=f}   = "float " ++ show f
    show String{tstring=s} = "string " ++ show s
    show Term{}            = "term"
    show Token{ttoken=t}   = show t

instance Unexpectable Token where
    xline = tline . head
    xshow = show . head


-- Token matching rules
sym :: Rule Token String
sym = Rule $ \case
    Sym{tsym=s}:ts -> Accept s ts
    ts             -> Reject ts

int :: Rule Token Int
int = Rule $ \case
    Int{tint=i}:ts -> Accept i ts
    ts             -> Reject ts

float :: Rule Token Double
float = Rule $ \case
    Float{tfloat=f}:ts -> Accept f ts
    ts                 -> Reject ts

string :: Rule Token String
string = Rule $ \case
    String{tstring=s}:ts -> Accept s ts
    ts                   -> Reject ts

term :: Rule Token ()
term = Rule $ \case
    Term{}:ts -> Accept () ts
    ts        -> Reject ts

token :: String -> Rule Token ()
token t = Rule $ \case
    Token{ttoken=t'}:ts | t == t' -> Accept () ts
    ts                            -> Reject ts


-- Tokenizing rules
tokSym :: Rule Char (Line -> Token)
tokSym = Rule $ \cs -> case span isAlphaNum cs of
    ("", cs') -> Reject cs'
    (s,  cs') -> Accept (Sym s) cs'

tokDigit :: Real n => n -> Rule Char n
tokDigit base = Rule $ \case
    c:cs | isBase c -> Accept (toBase c) cs
    cs              -> Reject cs
  where
    isBase c = isDigit c && toBase c < base
    toBase = fromIntegral . digitToInt

tokBase :: Real n => Rule Char n
tokBase = Rule $ \case
    '0':b:cs | elem b "bB" -> Accept 2  cs
    '0':b:cs | elem b "oO" -> Accept 8  cs
    '0':b:cs | elem b "xX" -> Accept 16 cs
    cs                     -> Accept 10 cs

tokExp :: Real n => Rule Char n
tokExp = Rule $ \case
    c:cs | elem c "eE" -> Accept 10 cs
    c:cs | elem c "pP" -> Accept 2 cs
    cs                 -> Reject cs

tokIntPart :: Real n => n -> Rule Char n
tokIntPart base = toInt <$> many1 (tokDigit base)
  where toInt = foldr1 (\a b -> a + b*base)

tokFracPart :: RealFloat n => n -> Rule Char n
tokFracPart base = toFrac <$ match '.' <*> many1 (tokDigit base)
  where toFrac = (/base) . foldr1 (\a b -> a + b/base)

tokExpPart :: RealFloat n => Rule Char n
tokExpPart = (**) <$> tokExp <*> tokIntPart 10 <|> pure 1

tokInt :: Rule Char (Line -> Token)
tokInt = Int <$> (tokBase >>= tokIntPart)

tokFloat :: Rule Char (Line -> Token)
tokFloat = Float <$> do
    base <- tokBase
    int_ <- tokIntPart base
    frac <- tokFracPart base
    exp_ <- tokExpPart
    return $ (int_ + frac) * exp_

tokEscape :: Int -> Int -> Rule Char Char
tokEscape base count = toChar <$> (sequence $ replicate count $ tokDigit base)
  where toChar = chr . foldr1 (\a b -> a + b*base)

tokCharPart :: Char -> Rule Char Char
tokCharPart q = Rule $ \case
    '\\':'\\':cs  -> Accept '\\' cs
    '\\':'\'':cs  -> Accept '\'' cs
    '\\':'\"':cs  -> Accept '\"' cs
    '\\':'f':cs   -> Accept '\f' cs
    '\\':'n':cs   -> Accept '\n' cs
    '\\':'r':cs   -> Accept '\r' cs
    '\\':'t':cs   -> Accept '\t' cs
    '\\':'v':cs   -> Accept '\v' cs
    '\\':'0':cs   -> Accept '\0' cs
    '\\':'b':cs   -> step (tokEscape  2 8) cs
    '\\':'o':cs   -> step (tokEscape  8 3) cs
    '\\':'d':cs   -> step (tokEscape 10 3) cs
    '\\':'x':cs   -> step (tokEscape 16 2) cs
    cs@('\\':_)   -> Reject cs
    cs@('\n':_)   -> Reject cs
    c:cs | c /= q -> Accept c cs
    cs            -> Reject cs

tokChar :: Rule Char (Line -> Token)
tokChar = (Int . ord) <$ match '\'' <*> tokCharPart '\'' <* match '\''

tokString :: Rule Char (Line -> Token)
tokString = String <$ match '\"' <*> many (tokCharPart '\"') <* match '\"'


tokenize :: Rule Char (Line -> Token)
tokenize = Rule $ \case
    '-':'>':cs              -> Accept (Token "->") cs
    '(':cs                  -> Accept (Token "(") cs
    ')':cs                  -> Accept (Token ")") cs
    '{':cs                  -> Accept (Token "{") cs
    '}':cs                  -> Accept (Token "}") cs
    '[':cs                  -> Accept (Token "[") cs
    ']':cs                  -> Accept (Token "]") cs
    ',':cs                  -> Accept (Token ",") cs
    c:cs     | elem c ";\n" -> Accept Term cs
    c:cs     | isSpace c    -> step tokenize cs
    cs@(c:_) | isAlpha c    -> step tokSym cs
    cs@(c:_) | isDigit c    -> step (tokFloat <|> tokInt) cs
    cs@('\'':_)             -> step tokChar cs
    cs@('\"':_)             -> step tokString cs
    cs                      -> Reject cs

toklines :: Rule Char Line
toklines = Rule $ \case
    '\n':cs -> Accept 1 cs
    cs      -> step (0 <$ tokenize) cs


lex :: String -> [Token]
lex cs = zipWith ($) tokens lines_
  where
    tokens = run (many tokenize) cs
    lines_ = scanl1 (+) $ run (many toklines) cs

