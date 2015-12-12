module Rule where

import Prelude hiding (repeat)
import Control.Applicative
import Data.Maybe
import Data.List hiding (repeat)


-- Rule definition and operations
newtype Rule t a = Rule { step :: [t] -> Result t a }

data Result t a = Accept a [t]
                | Reject   [t]

infixl 2 `step`

instance Functor (Rule t) where
    fmap f r = Rule $ \ts -> case step r ts of
        Accept a ts -> Accept (f a) ts
        Reject ts   -> Reject ts

instance Applicative (Rule t) where
    pure a = Rule $ Accept a

    f <*> a = Rule $ \ts -> case step f ts of
        Accept f ts -> f <$> a `step` ts
        Reject ts   -> Reject ts

instance Alternative (Rule t) where
    empty = Rule Reject

    a <|> b = Rule $ \ts -> case step a ts of
        Accept a ts -> Accept a ts
        Reject _    -> step b ts


look :: Rule t a -> Rule t a
look r = Rule $ \ts -> case step r ts of
    Accept a _  -> Accept a ts
    Reject   ts -> Reject ts


option :: Rule t a -> Rule t (Maybe a)
option r = Just <$> r <|> pure Nothing

repeat, repeat1 :: Rule t a -> Rule t [a]
repeat  r = (:) <$> r <*> repeat r <|> pure []
repeat1 r = (:) <$> r <*> repeat r

delimit, delimit1 :: Rule t a -> Rule t b -> Rule t [a]
delimit  r s = (:) <$> r <*> (s *> delimit r s <|> pure []) <|> pure []
delimit1 r s = (:) <$> r <*> (s *> delimit r s <|> pure [])

terminate, terminate1 :: Rule t a -> Rule t b -> Rule t [a]
terminate  r s = repeat  (r <* s)
terminate1 r s = repeat1 (r <* s)

separate, separate1 :: Rule t a -> Rule t b -> Rule t [a]
separate  r s = repeat s *> delimit  r (repeat1 s) <* repeat s
separate1 r s = repeat s *> delimit1 r (repeat1 s) <* repeat s


match :: Eq t => t -> Rule t t
match t = Rule $ \ts -> case ts of
    (t':ts) | t == t' -> Accept t' ts
    ts                -> Reject ts

matchN :: Eq t => [t] -> Rule t [t]
matchN ts = Rule $ \ts' -> case ts' of
    ts' | isPrefixOf ts ts' -> uncurry Accept $ splitAt (length ts) ts'
    ts'                     -> Reject ts'

matchIf :: (t -> Bool) -> Rule t t
matchIf p = Rule $ \ts -> case ts of
    (t:ts) | p t -> Accept t ts
    ts           -> Reject ts

matchMaybe :: (t -> Maybe a) -> Rule t a
matchMaybe f = Rule $ \ts -> case ts of
    (t:ts) | isJust (f t) -> Accept (fromJust (f t)) ts
    ts                    -> Reject ts


-- Unexpectable typeclass used for reporting errors
type Line = Int

class (Show t) => Unexpectable t where
    line :: [t] -> Line

    unexpected :: [t] -> a
    unexpected ts = error $ "unexpected " ++ case ts of
        (t:_) -> show t ++ " on line " ++ show (line ts + 1)
        _     -> "end of input"

instance Unexpectable Char where
    line = length . filter (== '\n')


run :: (Unexpectable t) => Rule t a -> [t] -> a
run r ts = case step r ts of
    Accept a [] -> a
    Accept _ ts -> unexpected $ handled ts
    Reject   ts -> unexpected $ handled ts
  where
    handled ts' = reverse $ take (length ts - length ts' + 1) ts

