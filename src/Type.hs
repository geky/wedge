module Type where

import Data.Maybe


-- Type definitions
data Type
    = Void
    | Type String
    | Array Type (Maybe Int)
    | Struct Struct
    | Func Struct Struct
    deriving Show

type Struct = [(Type, Maybe String)]


toStruct :: Type -> Struct
toStruct (Struct ys) = ys
toStruct Void        = []
toStruct y           = [(y, Nothing)]

fromStruct :: Struct -> Type
fromStruct []             = Void
fromStruct [(y, Nothing)] = y
fromStruct ys             = Struct ys

names :: Struct -> [String]
names = catMaybes . map snd

