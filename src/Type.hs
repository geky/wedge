module Type where

import Rule
import Lex
import Control.Applicative
import Data.List
import Data.Maybe

-- Type definitions
type Tuple = [(Type, Maybe String)]
data Type
    = Void
    | Type String
    | ArrayType Type (Maybe Int)
    | StructType Tuple
    | FuncType Tuple Tuple
    deriving Show

toTuple :: Type -> Tuple
toTuple (StructType ys) = ys
toTuple Void            = []
toTuple y               = [(y, Nothing)]

fromTuple :: Tuple -> Type
fromTuple []             = Void
fromTuple [(y, Nothing)] = y
fromTuple ys             = StructType ys


-- Type parsing
pType :: Rule Token Type
pType = pBase <**> pSuffixes
  where
    pBase = rule $ \case
        Symbol "void":ts -> accept Void ts
        Token "(":_      -> token "(" *> pNested <* token ")"
        Symbol s:ts      -> accept (Type s) ts
        _                -> reject

    pSuffixes = foldl (.) id <$> many pSuffix
    pSuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

    pNested = fromTuple <$> (pTuple <**> pNestSuffixes)
    pNestSuffixes = foldl (.) id <$> many pNestSuffix
    pNestSuffix = toFunc <$ token "->" <*> pTuple
      where toFunc rets args = [(FuncType args rets, Nothing)]

pTuple :: Rule Token Tuple
pTuple = delimited pVar (token ",")

pVar :: Rule Token (Type, Maybe String)
pVar = (,) <$> pType <*> optional symbol


-- Emitting definitions
emitArgs :: Tuple -> String
emitArgs = (\s -> "("++s++")") . \case
    [] -> "void"
    ys -> intercalate ", " $ map emitVar ys

emitMembers :: Tuple -> String
emitMembers = (\s -> "{"++s++"}") . intercalate " " . map ((++";") . emitVar)

emitType :: Type -> String
emitType = \case
    Void          -> "void"
    Type "int"    -> "int"
    Type "uint"   -> "unsigned"
    Type "float"  -> "float"
    Type t        -> t
    StructType y  -> "struct " ++ emitMembers y
    ArrayType y n -> emitType y ++ "[" ++ maybe "" show n ++ "]"
    FuncType y z  -> emitType (fromTuple z) ++ " (*)" ++ emitArgs y

emitVar :: (Type, Maybe String) -> String
emitVar (y, name) = case y of
    ArrayType y n -> emitType y ++ maybe "" (" "++) name
                  ++ "[" ++ maybe "" show n ++ "]"
    FuncType y z  -> emitType (fromTuple y) ++ " (*" ++ fromMaybe "" name ++ ")"
                  ++ emitArgs z
    y             -> emitType y ++ maybe "" (" "++) name

