module Parse where

import Prelude hiding (lex)
import Lex
import Rule
import Type


data PTree
  = PType String
  | PDecl [Type] String
  | PNop
  deriving Show

parseSym :: Rule Token String
parseSym (TSym sym:ts) = Accept sym ts
parseSym ts            = Reject ts

parseBaseType :: Rule Token Type
parseBaseType (TSym sym:ts) = Accept (Type sym) ts
parseBaseType (TLParen:ts)  = (TupleType ~$ parseType ~< match TRParen) ts
parseBaseType ts            = Reject ts

parseType :: Rule Token [Type]
parseType = delimit1 parseBaseType (match TTerm) ~~ mod 
  where
    mod y (TLParen:ts) = (funcmod y ~$ parseType ~< match TRParen) ts
    mod y (TLBrace:ts) = (const (arraymod y) ~$ match TRBrace) ts
    mod y ts           = Accept y ts

    funcmod y z = [FunctionType y z]
    arraymod y = [ArrayType y]

parseDecl :: Rule Token PTree
parseDecl = PDecl ~$ parseType ~* parseSym

parse :: [Token] -> [PTree]
parse = run $ separate parseDecl $ match TTerm

