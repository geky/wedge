module Emit where

import Parse


emit :: String -> WTree -> String
emit "h" = unlines . emitTree
emit "c" = unlines . emitTree
emit _   = undefined
