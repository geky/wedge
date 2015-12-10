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
  | TLBrace
  | TRBrace
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


lex :: String -> [Token]
lex = run $ repeat tokenize

