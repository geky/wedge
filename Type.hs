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
func, array, struct :: Rule Token Type
func   = FuncType <$> struct <* look (token "(") <*> struct
array  = ArrayType <$> base <* token "[" <*> option int <* token "]"
struct = tuple <$> delimit1 base term
  where tuple [y] = y
        tuple ys  = StructType ys

base :: Rule Token Type
base = Rule $ \ts -> case ts of
    Sym{tsym="void"}:ts -> Accept Void ts
    Sym{tsym=sym}:ts    -> Accept (Type sym) ts
    Token{tt="("}:ts    -> type_ <* token ")" `step` ts
    ts                  -> Reject ts

type_ :: Rule Token Type
type_ = func <|> array <|> struct


-- Type generators
genArgs :: Type -> String
genArgs y = (\s -> "(" ++ s ++ ")") $ case y of
    StructType ys -> intercalate ", " $ map genType ys
    y             -> genType y

genMembers :: Type -> String
genMembers (StructType ys) =
    (\s -> "{" ++ s ++ "}") $ intercalate " " $ map ((++";") . genType) ys

genType :: Type -> String
genType y = case y of
    Void                 -> "void"
    Type "int"           -> "int"
    Type "uint"          -> "uint"
    Type t               -> t
    StructType _         -> "struct " ++ genMembers y
    ArrayType y (Just n) -> genType y ++ "[" ++ show n ++ "]"
    ArrayType y _        -> genType y ++ "[]"
    FuncType y z         -> genType y ++ " (*)" ++ genArgs z

genDecl :: Type -> String -> String
genDecl y name = case y of
    ArrayType y (Just n) -> genType y ++ " " ++ name ++ "[" ++ show n ++ "]"
    ArrayType y _        -> genType y ++ " " ++ name ++ "[]"
    FuncType y z         -> genType y ++ " (*" ++ name ++ ")" ++ genArgs z
    y                    -> genType y ++ " " ++ name

