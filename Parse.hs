module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Lex
import Rule
import Type


data PTree
  = Decl Type String
  deriving Show


decl :: Rule Token PTree
decl = Decl <$> type_ <*> sym


parse :: [Token] -> [PTree]
parse = run $ separate decl term

