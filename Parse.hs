module Parse where

import Prelude hiding (lex)
import Lex
import Rule
import Type


data PTree
  = PType String
  | PDecl Type String
  | PNop
  deriving Show

parseSym :: Rule Token String
parseSym (TSym sym:ts) = Accept sym ts
parseSym ts            = Reject ts

parseType :: Rule Token Type
parseType = delimit1 parseType' (match TTerm)
  where
    parseType' (TSym "int"  :ts) = Accept IntType   ts
    parseType' (TSym "uint" :ts) = Accept UIntType  ts
    parseType' (TSym "float":ts) = Accept FloatType ts
    parseType' ts                = Reject ts

parseDecl :: Rule Token PTree
parseDecl = PDecl ~$ parseType ~* parseSym

parse :: [Token] -> [PTree]
parse = run $ separate parseDecl $ match TTerm

