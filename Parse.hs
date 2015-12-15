module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Lex
import Rule
import Type


-- Parse tree definitions
data PTree
  = Decl Type String
  deriving Show


-- Parsing rules
decl :: Rule Token PTree
decl = Decl <$> type_ <*> sym


parse :: [Token] -> [PTree]
parse = run $ separate decl term


-- Emitting definitions
emitPTree :: PTree -> String
emitPTree (Decl y name) = emitTypeDecl y name ++ ";"

