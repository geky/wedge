module Parse where

import Prelude hiding (lex)
import Lex
import Rule


data PTree
  = PType String
  | PDecl PTree String
  | PDeclList [PTree]
  | PNop
  deriving Show

parseSym :: Rule Token String
parseSym (TSym sym:ts) = Accept sym ts
parseSym ts            = Reject ts

parseType :: Rule Token PTree
parseType (TSym sym:ts@(TSym _:_)) = Accept (PType sym) ts
parseType ts                       = Reject ts

parseDecl :: Rule Token PTree
parseDecl = PDecl ~$ parseType ~* parseSym

parseDeclList :: Rule Token PTree
parseDeclList = PDeclList ~$ separate parseDecl parseDeclSep
  where
    parseDeclSep (TTerm:ts) = Accept () ts
    parseDeclSep ts         = Reject ts

parse :: [Token] -> PTree
parse = run parseDeclList

