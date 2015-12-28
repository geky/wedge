module Lex where

import Prelude hiding (lex)
import Control.Applicative
import Control.Monad
import Data.Char
import Rule


-- Token definitions
data Token
    = Symbol { tsymbol :: String,   tline :: Line }
    | Int    { tint    :: Int,      tline :: Line }
    | Float  { tfloat  :: Double,   tline :: Line }
    | String { tstring :: String,   tline :: Line }
    | Term   {                      tline :: Line }
    | Token  { ttoken  :: String,   tline :: Line }

instance Show Token where
    show Symbol{tsymbol=s} = "symbol " ++ show s
    show Int{tint=i}       = "int " ++ show i
    show Float{tfloat=f}   = "float " ++ show f
    show String{tstring=s} = "string " ++ show s
    show Term{}            = "term"
    show Token{ttoken=t}   = show t

instance Unexpectable Token where
    xline = tline . head
    xshow = show . head


-- Token matching rules
symbol :: Rule Token String
symbol = rule $ \case
    Symbol{tsymbol=s}:ts -> accept s ts
    _                    -> reject

int :: Rule Token Int
int = rule $ \case
    Int{tint=i}:ts -> accept i ts
    _              -> reject

float :: Rule Token Double
float = rule $ \case
    Float{tfloat=f}:ts -> accept f ts
    _                  -> reject

string :: Rule Token String
string = rule $ \case
    String{tstring=s}:ts -> accept s ts
    _                    -> reject

term :: Rule Token ()
term = rule $ \case
    Term{}:ts -> accept () ts
    _         -> reject

token :: String -> Rule Token ()
token t = rule $ \case
    Token{ttoken=t'}:ts | t == t' -> accept () ts
    _                             -> reject


-- Tokenizing rules
tokSym :: Rule Char (Line -> Token)
tokSym = Symbol <$> many1 (matchIf isAlphaNum)

tokDigit :: Num n => Int -> Rule Char n
tokDigit base = fromIntegral . digitToInt <$> matchIf isBase
  where isBase c = isDigit c && digitToInt c < base

tokSign :: Num n => Rule Char (n -> n)
tokSign = rule $ \case
    '-':cs -> accept negate cs
    '+':cs -> accept id cs
    cs     -> accept id cs

tokBase :: Rule Char Int
tokBase = rule $ \case
    '0':b:cs | elem b "bB" -> accept 2 cs
    '0':b:cs | elem b "oO" -> accept 8 cs
    '0':b:cs | elem b "xX" -> accept 16 cs
    cs                     -> accept 10 cs

tokInt :: Num n => Int -> Rule Char n
tokInt base = toInt base <$> many1 (tokDigit base)
  where
    toInt base' = foldr1 (\a b -> a + b*base)
      where base = fromIntegral base'

tokFrac :: Fractional n => Int -> Rule Char n
tokFrac base = toFrac base <$ match '.' <*> many1 (tokDigit base)
  where
    toFrac base' = (/base) . foldr1 (\a b -> a + b/base)
      where base = fromIntegral base'

tokExp :: Fractional n => Rule Char n
tokExp = (^^) <$> tokE <*> (tokSign <*> tokInt 10)
  where 
    tokE = rule $ \case
        c:cs | elem c "eE" -> accept 10 cs
        c:cs | elem c "pP" -> accept 2 cs
        _                  -> reject

tokNum :: Rule Char (Line -> Token)
tokNum = do
    sign <- tokSign
    base <- tokBase
    int  <- tokInt base
    let sign' = (*) $ fromIntegral $ sign 1
    let int'  = fromIntegral int

    rule $ \case
        c:_ | elem c ".pPeE" -> do
            frac <- tokFrac base <|> pure 0
            exp  <- tokExp <|> pure 1
            return $ Float (sign' ((int'+frac)*exp))
        _ -> do
            return $ Int (sign int)


tokEscape :: Int -> Int -> Rule Char Char
tokEscape count base = toChar <$> replicateM count (tokDigit base)
  where toChar = chr . foldr1 (\a b -> a + b*base)

tokC :: Char -> Rule Char Char
tokC q = rule $ \case
    '\\':'\\':cs  -> accept '\\' cs
    '\\':'\'':cs  -> accept '\'' cs
    '\\':'\"':cs  -> accept '\"' cs
    '\\':'f':cs   -> accept '\f' cs
    '\\':'n':cs   -> accept '\n' cs
    '\\':'r':cs   -> accept '\r' cs
    '\\':'t':cs   -> accept '\t' cs
    '\\':'v':cs   -> accept '\v' cs
    '\\':'0':cs   -> accept '\0' cs
    '\\':'b':cs   -> accept (8, 2)  cs >>= uncurry tokEscape
    '\\':'o':cs   -> accept (3, 8)  cs >>= uncurry tokEscape
    '\\':'d':cs   -> accept (3, 10) cs >>= uncurry tokEscape
    '\\':'x':cs   -> accept (2, 16) cs >>= uncurry tokEscape
    '\\':_        -> reject
    '\n':_        -> reject
    c:cs | c /= q -> accept c cs
    _             -> reject

tokChar :: Rule Char (Line -> Token)
tokChar = Int . ord <$ match '\'' <*> tokC '\'' <* match '\''

tokString :: Rule Char (Line -> Token)
tokString = String <$ match '\"' <*> many (tokC '\"') <* match '\"'

tokSingleComment :: Rule Char Line
tokSingleComment = 0 <$ matches "//" <* many (matchIf (/= '\n'))

tokMultiComment :: Rule Char Line
tokMultiComment = sum <$ matches "/*" <*> many comment <* matches "*/"
  where
    comment = rule $ \case
        '/':'*':_ -> tokMultiComment *> comment
        '*':'/':_ -> reject
        '\n':cs   -> accept 1 cs
        _:cs      -> accept 0 cs
        _         -> reject

tokenize :: Rule Char (Line -> Token)
tokenize = rule $ \case
    '(':cs              -> accept (Token "(") cs
    ')':cs              -> accept (Token ")") cs
    '{':cs              -> accept (Token "{") cs
    '}':cs              -> accept (Token "}") cs
    '[':cs              -> accept (Token "[") cs
    ']':cs              -> accept (Token "]") cs
    ',':cs              -> accept (Token ",") cs
    c:_ | isAlpha c     -> tokSym
    c:_ | isDigit c     -> tokNum
    '\'':_              -> tokChar
    '\"':_              -> tokString
    c:cs | elem c ";\n" -> accept Term cs
    '/':'/':_           -> tokSingleComment *> tokenize
    '/':'*':_           -> tokMultiComment  *> tokenize
    c:_ | isSpace c     -> matchAny *> tokenize
    _                   -> reject

tokLines :: Rule Char Line
tokLines = rule $ \case
    '\n':cs         -> accept 1 cs
    '/':'/':_       -> (+) <$> tokSingleComment <*> tokLines
    '/':'*':_       -> (+) <$> tokMultiComment  <*> tokLines
    c:_ | isSpace c -> matchAny *> tokLines
    _               -> 0 <$ tokenize


lex :: String -> [Token]
lex cs = zipWith ($) tokens (0:lines)
  where
    tokens = run (many tokenize) cs
    lines = scanl1 (+) $ run (many tokLines) cs

