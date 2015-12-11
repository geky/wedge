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

type_ :: Rule Token Type
type_ = func <|> array <|> struct
  where
    func  = FuncType <$> struct <* look TLParen <*> struct
    array = ArrayType <$> base <* match TLBrace <* match TRBrace

    struct = tuple <$> delimit1 base (match TTerm)
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
parse ts = run unexpected (separate decl (match TTerm)) ts
  where
    unexpected = undefined
--cs = error $ "unexpected " ++ what cs
--
--    what []        = "end of input"
--    what ts'@(t:_) = show t ++ " on line " ++ show n
--      where n = lexLines ts !! (length ts - length ts')

