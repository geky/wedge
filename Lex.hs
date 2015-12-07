module Lex where

import Prelude hiding (lex)
import Data.Char
import Rule

data Token
  = TSym String
  | TTerm
  | TLParen
  | TRParen
  | TLBlock
  | TRBlock
  deriving Show

tokenize :: Rule Char Token
tokenize ('(':cs) = (TLParen, cs)
tokenize (')':cs) = (TRParen, cs)
tokenize ('{':cs) = (TLBlock, cs)
tokenize ('}':cs) = (TRBlock, cs)

tokenize (c:cs) | elem c [',', ';', '\n', '\r'] = (TTerm, cs)
tokenize (c:cs) | isSpace c = tokenize cs

tokenize cs@(c:_) | isAlpha c = (TSym sym, cs')
  where (sym, cs') = span isAlphaNum cs

tokenize cs = unexpected cs


lex :: String -> [Token]
lex = run tokenize

