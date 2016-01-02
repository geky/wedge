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
pType = suffix pBase pSuffix
  where
    pBase = rule $ \case
        Symbol "void":ts -> accept Void ts
        Token "(":_      -> fromTuple <$> pNested
        Token "{":_      -> fromTuple <$> pStruct
        Symbol s:ts      -> accept (Type s) ts
        _                -> reject

    pSuffix = flip ArrayType <$ token "[" <*> optional int <* token "]"

    pNested = token "(" *> suffix pTuple pNestSuffix <* token ")"
    pNestSuffix = toFunc <$ token "->" <*> pTuple
      where toFunc rets args = [(FuncType args rets, Nothing)]

pStruct, pTuple :: Rule Token Tuple
pStruct = token "{" *> separated pVar term <* token "}"
pTuple = rule $ \case
    Token "(":_ -> token "(" *> pTuple <* token ")"
    _           -> delimited pVar (token ",")

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
    Void            -> "void"
    Type "int"      -> "int"
    Type "uint"     -> "unsigned"
    Type "float"    -> "double"
    Type t          -> t
    StructType ys   -> "struct " ++ emitMembers ys
    y               -> emitVar (y, Nothing)

emitVar :: (Type, Maybe String) -> String
emitVar (y, name) = case y of
    ArrayType y n -> emitType y ++ name' ++ "[" ++ n' ++ "]"
      where
        name' = fromMaybe "" ((" "++) <$> name)
        n'    = fromMaybe "" (show <$> n)
    FuncType y z -> rets ++ " (*" ++ name' ++ ")" ++ args
      where
        rets  = emitType (fromTuple y)
        args  = emitArgs z
        name' = fromMaybe "" name
    y -> emitType y ++ name'
      where
        name' = fromMaybe "" ((" "++) <$> name)


