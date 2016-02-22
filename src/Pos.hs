module Pos (FilePath, module Pos) where

import Prelude hiding (error)
import Control.Arrow
import Rule
import qualified Result as R


-- Position in a file
data Pos = Pos FilePath Line
  deriving Eq

type Line = Int


instance Show Pos where
    show (Pos "" (-1)) = "somewhere"
    show (Pos fp (-1)) = fp
    show (Pos "" l)    = "line " ++ show l
    show (Pos fp l)    = fp ++ " on line " ++ show l

instance Ord Pos where
    Pos fp1 l1 <= Pos fp2 l2 = (fp1, l1) <= (fp2, l2)

instance Monoid Pos where
    mempty = Pos "" (-1)
    mappend = const


-- Positional messages
data Msg = Msg Pos [String]

msg :: Pos -> String -> Msg
msg p ls = Msg p [ls]

instance Show Msg where
    show (Msg p ls) = unlines
      $ (show p ++ ":")
      : map (replicate 4 ' ' ++) ls

-- Msg specific results
type Result = R.Result Msg


-- General position things
begin :: FilePath -> Pos
begin fp = Pos fp 0

end :: FilePath -> Pos
end fp = Pos fp (-1)

next :: Char -> Pos -> Pos
next '\n' (Pos fp l) = Pos fp (l+1)
next _ p             = p


posString :: FilePath -> String -> [(Pos, Char)]
posString fp = uncurry zip <<< scanl (flip next) (begin fp) &&& id

posLines :: FilePath -> [String] -> [[(Pos, Char)]]
posLines = zipWith zip . positions

positions :: FilePath -> [[Pos]]
positions = map repeat . iterate (next '\n') . begin

pos :: Rule (Pos, t) (a -> (Pos, a))
pos = (,) . fst <$> current

-- Result overloading
ok :: b -> Result b
ok = R.ok

warning :: Pos -> String -> b -> Result b
warning p m b = R.warning (msg p m) b

error :: Pos -> String -> Result b
error p m = R.error (msg p m)

unexpected :: Show a => FilePath -> [(Pos, a)] -> Result b
unexpected fp []       = error (end fp) ("unexpected end of input")
unexpected _ ((p,a):_) = error p        ("unexpected " ++ show a)

expect :: Show a => FilePath -> Either [(Pos, a)] b -> Result b
expect fp = either (unexpected fp) ok

