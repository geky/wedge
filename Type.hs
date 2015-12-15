module Type where

import Rule
import Lex
import Control.Applicative
import Data.List

-- Type definitions
data Type
  = Void
  | Type String
  | StructType [Type]
  | ArrayType Type (Maybe Int)
  | FuncType Type Type
  deriving Show


-- Type rules
base :: Rule Token Type
base = Rule $ \ts -> case ts of
    Sym{tsym="void"}:ts -> Accept Void ts
    Sym{tsym=sym}:ts    -> Accept (Type sym) ts
    Token{tt="("}:ts    -> type_ <* token ")" `step` ts
    ts                  -> Reject ts

func, array, struct :: Rule Token Type
func   = FuncType <$> struct <* token "->" <*> struct
array  = ArrayType <$> base <* token "[" <*> option int <* token "]"
struct = tuple <$> delimit1 base term
  where tuple [y] = y
        tuple ys  = StructType ys

type_ :: Rule Token Type
type_ = func <|> array <|> struct


-- Emitting definitions
emitArgs :: Type -> String
emitArgs y = (\s -> "(" ++ s ++ ")") $ case y of
    StructType ys -> intercalate ", " $ map emitType ys
    y             -> emitType y

emitMembers :: Type -> String
emitMembers (StructType ys) =
    (\s -> "{" ++ s ++ "}") $ intercalate " " $ map ((++";") . emitType) ys

emitType :: Type -> String
emitType y = case y of
    Void                 -> "void"
    Type "int"           -> "int"
    Type "uint"          -> "unsigned"
    Type "float"         -> "float"
    Type t               -> t
    StructType _         -> "struct " ++ emitMembers y
    ArrayType y (Just n) -> emitType y ++ "[" ++ show n ++ "]"
    ArrayType y _        -> emitType y ++ "[]"
    FuncType y z         -> emitType z ++ " (*)" ++ emitArgs y

emitTypeDecl :: Type -> String -> String
emitTypeDecl y name = case y of
    ArrayType y (Just n) -> emitType y ++ " " ++ name ++ "[" ++ show n ++ "]"
    ArrayType y _        -> emitType y ++ " " ++ name ++ "[]"
    FuncType y z         -> emitType z ++ " (*" ++ name ++ ")" ++ emitArgs y
    y                    -> emitType y ++ " " ++ name

