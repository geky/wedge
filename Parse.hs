{-# LANGUAGE FlexibleInstances #-}

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
parseSym (TSym sym : ts) = (sym, ts)
parseSym ts = unexpected ts

parseType :: Rule Token PTree
parseType (TSym sym : ts@(TSym _ : _)) = (PType sym, ts)
parseType ts = unexpected ts

parseDecl :: Rule Token PTree
parseDecl = PDecl $~ parseType *~ parseSym

parseDeclList :: Rule Token [PTree]
parseDeclList []         = ([], [])
parseDeclList (TTerm:ts) = parseDeclList ts
parseDeclList ts         = parseDecl ~> parseDeclList' $ ts
  where
    parseDeclList' decl []          = ([decl], [])
    parseDeclList' decl (TTerm:ts') = (decl:) $~ parseDeclList $ ts'
    parseDeclList' _ ts' = unexpected ts'


parse :: [Token] -> PTree
parse ts = PDeclList $ fst $ parseDeclList ts

