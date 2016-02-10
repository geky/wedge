{-# LANGUAGE FlexibleInstances #-}

module Pos where

import Control.Arrow
import Data.List


-- Position in a file
type Positional a = (Pos, a)

data Pos = Pos FilePath Line Col
  deriving Eq

type Line = Int
type Col = Int

instance Show Pos where
    show (Pos fp l c) = intercalate ":"
        [ if null fp then "-" else fp
        , if l == maxBound then "-" else show (l+1)
        , if c == maxBound then "-" else show (c+1) ]

instance Ord Pos where
    Pos _ la ca <= Pos _ lb cb
        | la <  lb  = True
        | la == lb  = ca <= cb
        | otherwise = False

instance Monoid Pos where
    mempty = end ""
    mappend = min


start :: FilePath -> Pos
start fp = Pos fp 0 0

end :: FilePath -> Pos
end fp = Pos fp maxBound maxBound

nextLine, nextChar :: Pos -> Pos
nextLine (Pos fp l _) = Pos fp (l+1) 0
nextChar (Pos fp l c) = Pos fp l (c+1)

posString :: FilePath -> String -> [Positional Char]
posString fp = uncurry zip <<< scanl (flip next) (start fp) &&& id
  where next = \case '\n' -> nextLine; _ -> nextChar

posLines :: FilePath -> [String] -> [[Positional Char]]
posLines = zipWith zip . positions

positions :: FilePath -> [[Pos]]
positions = map (iterate nextChar) . iterate nextLine . start


showAt :: Pos -> [String] -> [String]
showAt p lines = (show p ++ ":") : map (replicate 4 ' ' ++) lines

errorAt :: Pos -> String -> b
errorAt p s = error $ unlines $ showAt p [s]

unexpected :: Show a => FilePath -> [Positional a] -> b
unexpected fp []       = errorAt (end fp) $ "unexpected end of input"
unexpected _ ((p,a):_) = errorAt p        $ "unexpected " ++ show a

expect :: Show a => FilePath -> Either [Positional a] b -> b
expect fp = \case
    Left []        -> errorAt (end fp) $ "unexpected end of input"
    Left ((p,a):_) -> errorAt p        $ "unexpected " ++ show a
    Right b        -> b

