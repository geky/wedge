module Emit where

import Parse


emit :: String -> Tree -> String
emit "h" = unlines . emitTree
emit "c" = unlines . emitTree
emit _   = undefined
