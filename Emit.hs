module Emit where

import Parse


emit :: String -> [PTree] -> String
emit "h" = unlines . emitTree
emit "c" = unlines . emitTree

