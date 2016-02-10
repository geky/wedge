module Lex where

import Prelude hiding (lex, exp, lines)
import Control.Applicative
import Control.Monad
import Data.Char
import Rule
import Pos


-- Token definitions
type Token = Positional Token'
data Token'
    = Symbol String
    | Op String Int
    | Int' Int
    | Float' Double
    | String String
    | Term
    | Token String
    deriving (Show, Eq)

incPrec :: Token' -> Token'
incPrec (Op s l) = Op s (l+1)
incPrec t        = t


-- Tokenizing rules
symbol :: Rule Pos Char Token'
symbol = Symbol <$> many1 (matchIf isAlphaNum)

isOp :: Char -> Bool
isOp = flip elem "~!@#$%^&*-=+<>?/\\."

op :: Rule Pos Char Token'
op = flip Op 0 <$> many1 (matchIf isOp)

digit :: Num n => Int -> Rule Pos Char n
digit base = fromIntegral . digitToInt <$> matchIf isBase
  where isBase c = isDigit c && digitToInt c < base

sign :: Num n => Rule Pos Char (n -> n)
sign = rule $ \case
    '-':_ -> accept 1 negate
    '+':_ -> accept 1 id
    _     -> accept 0 id

base :: Rule Pos Char Int
base = rule $ \case
    '0':b:_ | elem b "bB" -> accept 2 2
    '0':b:_ | elem b "oO" -> accept 2 8
    '0':b:_ | elem b "xX" -> accept 2 16
    _                     -> accept 0 10

int :: Num n => Int -> Rule Pos Char n
int base = foldr1 (\a b -> a + b*base') <$> many1 (digit base)
  where base' = fromIntegral base

frac :: Fractional n => Int -> Rule Pos Char n
frac base = (/base') . foldr1 (\a b -> a + b/base')
  <$  match '.'
  <*> many1 (digit base)
  where base' = fromIntegral base

exp :: Fractional n => Rule Pos Char n
exp = (^^) <$> e <*> (sign <*> int 10)
  where 
    e = rule $ \case
        c:_ | elem c "eE" -> accept 1 10
        c:_ | elem c "pP" -> accept 1 2
        _                 -> reject

num :: Rule Pos Char Token'
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
            return $ Float' (sign' ((int'+frac)*exp))
        _ -> do
            return $ Int' (sign int)


escape :: Int -> Int -> Rule Pos Char Char
escape count base = chr . foldr1 (\a b -> a + b*base)
  <$> replicateM count (digit base)

character :: Char -> Rule Pos Char Char
character q = rule $ \case
    '\\':'\\':_  -> accept 2 '\\'
    '\\':'\'':_  -> accept 2 '\''
    '\\':'\"':_  -> accept 2 '\"'
    '\\':'f':_   -> accept 2 '\f'
    '\\':'n':_   -> accept 2 '\n'
    '\\':'r':_   -> accept 2 '\r'
    '\\':'t':_   -> accept 2 '\t'
    '\\':'v':_   -> accept 2 '\v'
    '\\':'0':_   -> accept 2 '\0'
    '\\':'b':_   -> accept 2 (8, 2)  >>= uncurry escape
    '\\':'o':_   -> accept 2 (3, 8)  >>= uncurry escape
    '\\':'d':_   -> accept 2 (3, 10) >>= uncurry escape
    '\\':'x':_   -> accept 2 (2, 16) >>= uncurry escape
    '\\':_       -> reject
    '\n':_       -> reject
    c:_ | c /= q -> accept 1 c
    _            -> reject

char :: Rule Pos Char Token'
char = Int' . ord <$ match '\'' <*> character '\'' <* match '\''

string :: Rule Pos Char Token'
string = String <$ match '\"' <*> many (character '\"') <* match '\"'

singleComment :: Rule Pos Char Line
singleComment = 0 <$ matches "//" <* many (matchIf (/= '\n'))

multiComment :: Rule Pos Char Line
multiComment = sum <$ matches "/*" <*> many comment <* matches "*/"
  where
    comment = rule $ \case
        '/':'*':_ -> multiComment *> comment
        '*':'/':_ -> reject
        '\n':_    -> accept 1 1
        _:_       -> accept 1 0
        _         -> reject


tokenize :: Rule Pos Char Token'
tokenize = rule $ \case
    '-':'>':_          -> accept 2 (Token "->")
    '=':_              -> accept 1 (Token "=")
    '(':_              -> accept 1 (Token "(")
    ')':_              -> accept 1 (Token ")")
    '{':_              -> accept 1 (Token "{")
    '}':_              -> accept 1 (Token "}")
    '[':_              -> accept 1 (Token "[")
    ']':_              -> accept 1 (Token "]")
    ',':_              -> accept 1 (Token ",")
    '.':_              -> accept 1 (Token ".")
    c:_ | isAlpha c    -> symbol
    c:_ | isOp c       -> op
    c:_ | isDigit c    -> num
    '\'':_             -> char
    '\"':_             -> string
    c:_ | elem c ";\n" -> accept 1 Term
    '/':'/':_          -> singleComment *> tokenize
    '/':'*':_          -> multiComment  *> tokenize
    c:_ | isSpace c    -> accept 1 incPrec <*> tokenize
    _                  -> reject


-- Lexing entry point
lex :: FilePath -> [String] -> [Token]
lex fp = expect fp . run (many $ at tokenize) . posString fp . unlines

