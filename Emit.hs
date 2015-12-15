module Emit where

import Parse


emit :: String -> [PTree] -> String
emit "h" = concat . map ((++"\n") . emitPTree)
emit "c" = concat . map ((++"\n") . emitPTree)

