{-# LANGUAGE FlexibleInstances #-}

module File where

import Data.List


-- Position in a file
data Pos = Pos FilePath Line Col
  deriving Eq

type Line = Int
type Col = Int

instance Show Pos where
    show (Pos fp l c) = intercalate ":"
        [ fp
        , if l == maxBound then "-" else show (l+1)
        , if c == maxBound then "-" else show (c+1) ]

instance Ord Pos where
    Pos _ la ca <= Pos _ lb cb
        | la <  lb  = True
        | la == lb  = ca <= cb
        | otherwise = False

class Positional a where
    getPos :: a -> Pos

instance Positional (a, Pos) where
    getPos = snd


start :: FilePath -> Pos
start fp = Pos fp 0 0

end :: FilePath -> Pos
end fp = Pos fp maxBound maxBound

nextLine, nextChar :: Pos -> Pos
nextLine (Pos fp l _) = Pos fp (l+1) 0
nextChar (Pos fp l c) = Pos fp l (c+1)


posString :: FilePath -> String -> [Pos]
posString fp = scanl (flip next) (start fp)
  where next = \case '\n' -> nextLine; _ -> nextChar

posLines :: FilePath -> [String] -> [Pos]
posLines fp = posString fp . unlines


showAt :: Pos -> [String] -> [String]
showAt p lines = (show p ++ ":") : map (replicate 4 ' ' ++) lines

errorAt :: Pos -> String -> b
errorAt p s = error $ unlines $ showAt p [s]

unexpected :: (Positional a) => FilePath -> (a -> String) -> [a] -> b
unexpected fp _ []   = errorAt (end fp)   $ "unexpected end of input"
unexpected _ f (a:_) = errorAt (getPos a) $ "unexpected " ++ f a

