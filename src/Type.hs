module Type where

import Rule
import Lex
import Control.Applicative
import Data.List
import Data.Maybe

-- Type definitions
data Type
    = Void
    | Type String
    | StructType [Type]
    | ArrayType Type (Maybe Int)
    | FuncType Type Type
    deriving (Show, Eq)

data Var = V
    { vtype :: Maybe Type
    , vname :: Maybe String
    , vline :: Line
    }
    deriving Show

instance Unexpectable Var where
    xline = vline . head
    xshow = show . head


-- Type rules
pBase :: Rule Token Type
pBase = rule $ \case
    Symbol{tsymbol="void"}:ts -> accept Void ts
    Symbol{tsymbol=s}:ts      -> accept (Type s) ts
    Token{ttoken="("}:_       -> token "(" *> pTuple <* token ")"
    _                         -> reject

pTuple :: Rule Token Type
pTuple = toTuple <$> delimited1 pBase (token ",")
  where toTuple []  = Void
        toTuple [y] = y
        toTuple ys  = StructType ys

pFuncSuffix, pArraySuffix :: Rule Token (Type -> Type)
pFuncSuffix  = flip FuncType  <$ token "(" <*> pTuple       <* token ")"
pArraySuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

pSuffix :: Rule Token (Type -> Type)
pSuffix = pFuncSuffix <|> pArraySuffix <|> pure id

pType :: Rule Token Type
pType = pTuple <**> pSuffix


-- TODO fold this into another function?
pFuncVar :: Rule Token (Type, String)
pFuncVar = toFunc <$> pType <*> symbol <* token "(" <*> pType <* token ")"
  where toFunc ret name arg = (FuncType ret arg, name)

pVar :: Rule Token Var
pVar = toVar <$> current <*> pType <*> optional symbol <*> pSuffix
  where toVar t base name suff = V (Just $ suff base) name (tline t)


-- Emitting definitions
emitArgs :: Type -> String
emitArgs = (\s -> "("++s++")") . \case
    Void          -> "void"
    StructType ys -> intercalate ", " $ map emitType ys
    y             -> emitType y

emitMembers :: [Type] -> String
emitMembers = (\s -> "{"++s++"}") . intercalate " " . map ((++";") . emitType)

emitType :: Type -> String
emitType = \case
    Void          -> "void"
    Type "int"    -> "int"
    Type "uint"   -> "unsigned"
    Type "float"  -> "float"
    Type t        -> t
    StructType y  -> "struct " ++ emitMembers y
    ArrayType y n -> emitType y ++ "[" ++ maybe "" show n ++ "]"
    FuncType y z  -> emitType y ++ " (*)" ++ emitArgs z

emitTypeDecl :: Type -> String -> String
emitTypeDecl y name = case y of
    ArrayType y (Just n) -> emitType y ++ " " ++ name ++ "[" ++ show n ++ "]"
    ArrayType y Nothing  -> emitType y ++ " " ++ name ++ "[]"
    FuncType y z         -> emitType y ++ " (*" ++ name ++ ")" ++ emitArgs z
    y                    -> emitType y ++ " " ++ name

emitVar :: Var -> String
emitVar V{vtype=Just y, vname=name} = case y of
    ArrayType y n -> emitType y ++ maybe "" (" "++) name
                  ++ "[" ++ maybe "" show n ++ "]"
    FuncType y z  -> emitType y ++ " (*" ++ fromMaybe "" name ++ ")"
                  ++ emitArgs z
    y             -> emitType y ++ maybe "" (" "++) name

emitVar _ = undefined
