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


type_ :: Rule Token Type
type_ = func <|> array <|> struct
  where
    func  = FuncType <$> struct <* look (token "(") <*> struct
    array = ArrayType <$> base <* token "[" <* token "]"

    struct = tuple <$> delimit1 base tterm
      where
        tuple [y] = y
        tuple ys  = StructType ys

    base = Rule $ \ts -> case ts of
        Sym{sym="void"}:ts -> Accept Void ts
        Sym{sym=sym}:ts    -> Accept (Type sym) ts
        Token{tok="("}:ts  -> type_ <* token ")" `step` ts
        ts                 -> Reject ts
        
decl :: Rule Token PTree
decl = PDecl <$> type_ <*> tsym


parse :: [Token] -> [PTree]
parse = run unexpected $ separate decl tterm

