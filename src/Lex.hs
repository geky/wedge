module Lex where

import Prelude hiding (lex, exp, lines)
import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Rule
import Pos


-- Token definitions
data Token
    = Symbol Int String
    | Op Int String
    | Int' Int
    | Float' Double
    | String String
    | Term
    | Token String
    deriving Show

incPrec :: Token -> Token
incPrec (Symbol l s) = Symbol (l+1) s
incPrec (Op l s)     = Op (l+1) s
incPrec t            = t

instance Eq Token where
    Symbol _ a == Symbol _ b = a == b
    Op _ a     == Op _ b     = a == b
    Int' a     == Int' b     = a == b
    Float' a   == Float' b   = a == b
    String a   == String b   = a == b
    Term       == Term       = True
    Token a    == Token b    = a == b
    _          == _          = False


-- Tokenizing rules
symbol :: Rule (Pos, Char) (Pos, Token)
symbol = overM $ Symbol 0 <$> many1 (matchIf isAlphaNum)

isOp :: Char -> Bool
isOp = flip elem "~!@#$%^&*-=+<>?/\\."

op :: Rule (Pos, Char) (Pos, Token)
op = overM $ Op 0 <$> many1 (matchIf isOp)

digit :: Num n => Int -> Rule Char n
digit base = fromIntegral . digitToInt <$> matchIf isBase
  where isBase c = isDigit c && digitToInt c < base

sign :: Num n => Rule Char (n -> n)
sign = rule $ \case
    '-':_ -> accept 1 negate
    '+':_ -> accept 1 id
    _     -> accept 0 id

base :: Rule Char Int
base = rule $ \case
    '0':b:_ | elem b "bB" -> accept 2 2
    '0':b:_ | elem b "oO" -> accept 2 8
    '0':b:_ | elem b "xX" -> accept 2 16
    _                     -> accept 0 10

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
        c:_ | elem c "eE" -> accept 1 10
        c:_ | elem c "pP" -> accept 1 2
        _                 -> reject

num :: Rule (Pos, Char) (Pos, Token)
num = overM $ do
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


escape :: Int -> Int -> Rule Char Char
escape count base = chr . foldr1 (\a b -> a + b*base)
  <$> replicateM count (digit base)

character :: Char -> Rule Char Char
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
    '\\':'b':_   -> accept 2 () >> escape 8 2
    '\\':'o':_   -> accept 2 () >> escape 3 8
    '\\':'d':_   -> accept 2 () >> escape 3 10
    '\\':'x':_   -> accept 2 () >> escape 2 16
    '\\':_       -> reject
    '\n':_       -> reject
    c:_ | c /= q -> accept 1 c
    _            -> reject

char :: Rule (Pos, Char) (Pos, Token)
char = overM $ Int'
    <$ match '\'' <*> (ord <$> character '\'') <* match '\''

string :: Rule (Pos, Char) (Pos, Token)
string = overM $ String
    <$ match '\"' <*> many (character '\"') <* match '\"'

singleComment :: Rule (Pos, Char) (Pos, String)
singleComment = overM $ matches "//" *> many (matchIf (/= '\n'))

multiComment :: Rule (Pos, Char) (Pos, String)
multiComment = overM $ comment
  where
    comment = rule $ \case
        '/':'*':_ -> matches "/*" *> (concat <$> many comment) <* matches "*/"
        '*':'/':_ -> reject
        c:_       -> accept 1 [c]
        _         -> reject


token :: String -> Rule (Pos, Char) (Pos, Token)
token t = overM $ Token <$> matches t

tokenize :: Rule (Pos, Char) (Pos, Token)
tokenize = rule $ (.snd.unzip) $ \case
    '-':'>':_          -> token "->"
    '=':_              -> token "="
    '(':_              -> token "("
    ')':_              -> token ")"
    '{':_              -> token "{"
    '}':_              -> token "}"
    '[':_              -> token "["
    ']':_              -> token "]"
    ',':_              -> token ","
    '.':_              -> token "."
    '/':'/':_          -> singleComment *> tokenize
    '/':'*':_          -> multiComment  *> tokenize
    c:_ | isAlpha c    -> symbol
    c:_ | isOp c       -> op
    c:_ | isDigit c    -> num
    '\'':_             -> char
    '\"':_             -> string
    c:_ | elem c ";\n" -> overM $ accept 1 Term
    c:_ | isSpace c    -> accept 1 (second incPrec) <*> tokenize
    _                  -> reject

-- Lexing entry point
lex :: FilePath -> [String] -> Result [(Pos, Token)]
lex fp = expect fp . run (many tokenize) . posString fp . unlines

