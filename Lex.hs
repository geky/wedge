module Lex where

import Prelude hiding (lex, repeat)
import Data.Char
import Rule

data Token
  = TSym String
  | TTerm
  | TLParen
  | TRParen
  | TLBlock
  | TRBlock
  deriving (Show, Eq)


tokenize :: Rule Char Token

tokenize ('(':cs) = Accept TLParen cs
tokenize (')':cs) = Accept TRParen cs
tokenize ('{':cs) = Accept TLBlock cs
tokenize ('}':cs) = Accept TRBlock cs

tokenize (c:cs) | elem c ",;\n\r" = Accept TTerm cs
tokenize (c:cs) | isSpace c = tokenize cs

tokenize cs@(c:_) | isAlpha c = Accept (TSym sym) cs'
  where (sym, cs') = span isAlphaNum cs

tokenize ts = Reject ts


lex :: String -> [Token]
lex = run $ repeat tokenize

