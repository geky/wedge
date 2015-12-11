module Lex where

import Prelude hiding (lex, repeat)
import Data.Char
import Rule
import Control.Monad


-- Line numbers in file
type Line = Int


-- Token type and associated rules
data Token
    = TSym      String  --Line
    | TTerm             --Line
    | TLParen           --Line
    | TRParen           --Line
    | TLBlock           --Line
    | TRBlock           --Line
    | TLBrace           --Line
    | TRBrace           --Line
    deriving (Show, Eq)


tokenize :: Rule Char Token
tokenize = Rule $ \t -> case t of
    '(':cs -> Accept TLParen cs
    ')':cs -> Accept TRParen cs
    '{':cs -> Accept TLBlock cs
    '}':cs -> Accept TRBlock cs
    '[':cs -> Accept TLBrace cs
    ']':cs -> Accept TRBrace cs

    c:cs | elem c ",;\n\r" -> Accept TTerm cs
    c:cs | isSpace c -> step tokenize cs

    c:cs | isAlpha c -> Accept (TSym sym) cs'
      where (sym, cs') = span isAlphaNum (c:cs)

    cs -> Reject cs

tokenLine :: Rule Char Int
tokenLine = Rule $ \t -> case t of
    '\n':cs -> Accept 1 cs
    cs      -> case step tokenize cs of
        Accept _ cs -> Accept 0 cs
        Reject   cs -> Reject   cs


lex :: String -> [Token]
lex cs = run unexpected (repeat tokenize) cs
  where
    unexpected cs = error $ "unexpected " ++ what cs

    what []        = "end of input"
    what cs'@(c:_) = show c ++ " on line " ++ show n
      where n = length $ filter (== '\n') $ take (length cs - length cs') cs

lexLines :: String -> [Int]
lexLines = scanl (+) 0 . run undefined (repeat tokenLine)

