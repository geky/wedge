module Type where


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

