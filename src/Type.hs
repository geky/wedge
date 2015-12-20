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
    | ArrayType Type Int
    | FuncType Type Type
    deriving Show


-- Type rules
pBase :: Rule Token Type
pBase = Rule $ \case
    Symbol{tsymbol="void"}:ts -> Accept Void ts
    Symbol{tsymbol=s}:ts      -> Accept (Type s) ts
    ts@(Token{ttoken="("}:_)  -> step (paren pType) ts
    ts                        -> Reject ts

pStruct :: Rule Token Type
pStruct = toStruct <$> delimited1 pBase (token ",")
  where
    toStruct [y] = y
    toStruct ys  = StructType ys

pType :: Rule Token Type
pType = func <|> array <|> pStruct
  where
    func  = FuncType  <$> pStruct <*> paren pStruct
    array = ArrayType <$> pStruct <*> brace int

-- TODO fold this into another function?
pFuncVar :: Rule Token (Type, String)
pFuncVar = toFunc <$> pType <*> symbol <*> paren pType
  where toFunc ret name arg = (FuncType ret arg, name)

pVar :: Rule Token (Type, String)
pVar = func <|> array <|> ((,) <$> pType <*> symbol)
  where
    func  = toFunc  <$> pType <*> symbol <*> paren pType
    array = toArray <$> pType <*> symbol <*> brace int
    toFunc  ret name arg   = (FuncType ret arg, name)
    toArray ret name count = (ArrayType ret count, name)


-- Emitting definitions
emitArgs :: Type -> String
emitArgs y = (\s -> "(" ++ s ++ ")") $ case y of
    StructType ys -> intercalate ", " $ map emitType ys
    y             -> emitType y

emitMembers :: Type -> String
emitMembers y = 
    (\s -> "{" ++ s ++ "}") $ intercalate " " $ map ((++";") . emitType) ys
  where StructType ys = y

emitType :: Type -> String
emitType = \case
    Void             -> "void"
    Type "int"       -> "int"
    Type "uint"      -> "unsigned"
    Type "float"     -> "float"
    Type t           -> t
    y@(StructType _) -> "struct " ++ emitMembers y
    ArrayType y n    -> emitType y ++ "[" ++ show n ++ "]"
    FuncType y z     -> emitType y ++ " (*)" ++ emitArgs z

emitTypeDecl :: Type -> String -> String
emitTypeDecl y name = case y of
    ArrayType y n -> emitType y ++ " " ++ name ++ "[" ++ show n ++ "]"
    FuncType y z  -> emitType y ++ " (*" ++ name ++ ")" ++ emitArgs z
    y             -> emitType y ++ " " ++ name

