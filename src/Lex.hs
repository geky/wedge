module Lex where

import Prelude hiding (lex, exp)
import Control.Applicative
import Control.Monad
import Data.Char
import Rule


-- Token definitions
data Token
    = Symbol String
    | Op String Int
    | Int Int
    | Float Double
    | String String
    | Term
    | Token String
    deriving (Show, Eq)

incp :: Token -> Token
incp (Op s p) = Op s (p+1)
incp t        = t


-- Tokenizing rules
symbol :: Rule Char Token
symbol = Symbol <$> many1 (matchIf isAlphaNum)

isOp :: Char -> Bool
isOp = flip elem "~!@#$%^&*-=+<>?/\\."

op :: Rule Char Token
op = flip Op 0 <$> many1 (matchIf isOp)

digit :: Num n => Int -> Rule Char n
digit base = fromIntegral . digitToInt <$> matchIf isBase
  where isBase c = isDigit c && digitToInt c < base

sign :: Num n => Rule Char (n -> n)
sign = rule $ \case
    '-':cs -> accept negate cs
    '+':cs -> accept id cs
    cs     -> accept id cs

base :: Rule Char Int
base = rule $ \case
    '0':b:cs | elem b "bB" -> accept 2 cs
    '0':b:cs | elem b "oO" -> accept 8 cs
    '0':b:cs | elem b "xX" -> accept 16 cs
    cs                     -> accept 10 cs

int :: Num n => Int -> Rule Char n
int base = foldr1 (\a b -> a + b*base') <$> many1 (digit base)
  where base' = fromIntegral base

frac :: Fractional n => Int -> Rule Char n
frac base = (/base') . foldr1 (\a b -> a + b/base')
  <$  match '.'
  <*> many1 (digit base)
  where base' = fromIntegral base

exp :: Fractional n => Rule Char n
exp = (^^) <$> e <*> (sign <*> int 10)
  where 
    e = rule $ \case
        c:cs | elem c "eE" -> accept 10 cs
        c:cs | elem c "pP" -> accept 2 cs
        _                  -> reject

num :: Rule Char Token
num = do
    sign <- sign
    base <- base
    int  <- int base
    let sign' = (*) $ fromIntegral $ sign 1
    let int'  = fromIntegral int

    rule $ \case
        c:_ | elem c ".pPeE" -> do
            frac <- frac base <|> pure 0
            exp  <- exp <|> pure 1
            return $ Float (sign' ((int'+frac)*exp))
        _ -> do
            return $ Int (sign int)


escape :: Int -> Int -> Rule Char Char
escape count base = chr . foldr1 (\a b -> a + b*base)
  <$> replicateM count (digit base)

c :: Char -> Rule Char Char
c q = rule $ \case
    '\\':'\\':cs  -> accept '\\' cs
    '\\':'\'':cs  -> accept '\'' cs
    '\\':'\"':cs  -> accept '\"' cs
    '\\':'f':cs   -> accept '\f' cs
    '\\':'n':cs   -> accept '\n' cs
    '\\':'r':cs   -> accept '\r' cs
    '\\':'t':cs   -> accept '\t' cs
    '\\':'v':cs   -> accept '\v' cs
    '\\':'0':cs   -> accept '\0' cs
    '\\':'b':cs   -> accept (8, 2)  cs >>= uncurry escape
    '\\':'o':cs   -> accept (3, 8)  cs >>= uncurry escape
    '\\':'d':cs   -> accept (3, 10) cs >>= uncurry escape
    '\\':'x':cs   -> accept (2, 16) cs >>= uncurry escape
    '\\':_        -> reject
    '\n':_        -> reject
    c:cs | c /= q -> accept c cs
    _             -> reject

char :: Rule Char Token
char = Int . ord <$ match '\'' <*> c '\'' <* match '\''

string :: Rule Char Token
string = String <$ match '\"' <*> many (c '\"') <* match '\"'

singleComment :: Rule Char Line
singleComment = 0 <$ matches "//" <* many (matchIf (/= '\n'))

multiComment :: Rule Char Line
multiComment = sum <$ matches "/*" <*> many comment <* matches "*/"
  where
    comment = rule $ \case
        '/':'*':_ -> multiComment *> comment
        '*':'/':_ -> reject
        '\n':cs   -> accept 1 cs
        _:cs      -> accept 0 cs
        _         -> reject

tokenize :: Rule Char Token
tokenize = rule $ \case
    '-':'>':cs          -> accept (Token "->") cs
    '=':cs              -> accept (Token "=") cs
    '(':cs              -> accept (Token "(") cs
    ')':cs              -> accept (Token ")") cs
    '{':cs              -> accept (Token "{") cs
    '}':cs              -> accept (Token "}") cs
    '[':cs              -> accept (Token "[") cs
    ']':cs              -> accept (Token "]") cs
    ',':cs              -> accept (Token ",") cs
    '.':cs              -> accept (Token ".") cs
    c:_ | isAlpha c     -> symbol
    c:_ | isOp c        -> op
    c:_ | isDigit c     -> num
    '\'':_              -> char
    '\"':_              -> string
    c:cs | elem c ";\n" -> accept Term cs
    '/':'/':_           -> singleComment *> tokenize
    '/':'*':_           -> multiComment  *> tokenize
    c:_ | isSpace c     -> incp <$ matchAny <*> tokenize
    _                   -> reject

tokLines :: Rule Char Line
tokLines = rule $ \case
    '\n':cs         -> accept 1 cs
    '/':'/':_       -> (+) <$> singleComment <*> tokLines
    '/':'*':_       -> (+) <$> multiComment  <*> tokLines
    c:_ | isSpace c -> matchAny *> tokLines
    _               -> 0 <$ tokenize


charLines :: String -> [Line]
charLines = (0:) . scanl1 (+) . map (\c -> if c == '\n' then 1 else 0)

lex :: String -> [Token]
lex cs = run (many tokenize) cs (charLines cs)

lexLines :: String -> [Line]
lexLines cs = (0:) $ scanl1 (+) $ run (many tokLines) cs (charLines cs)

