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
pBase = Rule $ \case
    Symbol{tsymbol="void"}:ts -> Accept Void ts
    Symbol{tsymbol=s}:ts      -> Accept (Type s) ts
    Token{ttoken="("}:ts      -> step (pTuple <* token ")") ts
    ts                        -> Reject ts

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
pFuncVar = toFunc <$> pType <*> symbol <*> paren pType
  where toFunc ret name arg = (FuncType ret arg, name)

pVar :: Rule Token Var
pVar = toVar <$> line <*> pType <*> optional symbol <*> pSuffix
  where toVar line base name suff = V (Just $ suff base) name line

--pVar :: Rule Token (Type, String)
--pVar = do
--    base <- pType
--    name <- symbol
--    suff <- pSuffix
--    return (suff base, name)


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
    FuncType y z  -> emitType y ++ " (*" ++ maybe "" id name ++ ")"
                  ++ emitArgs z
    y             -> emitType y ++ maybe "" (" "++) name

emitVar _ = undefined
