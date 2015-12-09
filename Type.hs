module Type where

data Type
  = Type String
  | TupleType [Type]
  | ArrayType [Type]
  | FunctionType [Type] [Type]
  deriving Show

