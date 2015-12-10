module Type where

data Type
  = Void
  | Type String
  | StructType [Type]
  | ArrayType Type
  | FuncType Type Type
  deriving Show

