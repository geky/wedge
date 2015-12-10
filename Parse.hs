module Parse (PTree, parse) where

import Prelude hiding (lex)
import Control.Applicative
import Lex
import Rule
import Type


data PTree
  = PType String
  | PDecl Type String
  | PNop
  deriving Show

sym :: Rule Token String
sym = Rule $ \t -> case t of
    TSym sym:ts -> Accept sym ts
    ts          -> Reject ts

term :: Rule Token Token
term = match TTerm

type_ :: Rule Token Type
type_ = func <|> array <|> struct
  where
    func  = FuncType <$> struct <* match TLParen <*> struct <* match TRParen
    array = ArrayType <$> base <* match TLBrace <* match TRBrace

    struct = tuple <$> delimit1 base term
      where
        tuple [y] = y
        tuple ys  = StructType ys

    base = Rule $ \t -> case t of
        TSym "void":ts -> Accept Void ts
        TSym sym:ts    -> Accept (Type sym) ts
        TLParen:ts     -> type_ <* match TRParen `step` ts
        ts             -> Reject ts
        
decl :: Rule Token PTree
decl = PDecl <$> type_ <*> sym

parse :: [Token] -> [PTree]
parse = run $ separate decl term

