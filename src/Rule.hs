module Rule where

import Control.Applicative
import Data.Maybe


-- Rule definition and operations
newtype Rule t a = Rule { step :: [t] -> Result t a }

data Result t a = Accept a [t]
                | Reject   [t]


instance Functor (Rule t) where
    fmap f r = Rule $ \ts -> case step r ts of
        Accept a ts -> Accept (f a) ts
        Reject ts   -> Reject ts

instance Applicative (Rule t) where
    pure a = Rule $ Accept a

    f <*> a = Rule $ \ts -> case step f ts of
        Accept f ts -> step (f <$> a) ts
        Reject ts   -> Reject ts

instance Alternative (Rule t) where
    empty = Rule Reject

    a <|> b = Rule $ \ts -> case step a ts of
        Accept a ts -> Accept a ts
        Reject _    -> step b ts

instance Monad (Rule t) where
    return = pure
    fail _ = empty

    a >>= b = Rule $ \ts -> case step a ts of
        Accept a ts -> step (b a) ts
        Reject ts   -> Reject ts


-- many is already defined in Control.Applicative
many1 :: Alternative f => f a -> f [a]
many1 = some

delimited, delimited1 :: Alternative f => f a -> f b -> f [a]
delimited  r s = delimited1 r s <|> pure []
delimited1 r s = (:) <$> r <*> many (s *> r)

terminated, terminated1 :: Alternative f => f a -> f b -> f [a]
terminated  r s = many  (r <* s)
terminated1 r s = many1 (r <* s)

separated, separated1 :: Alternative f => f a -> f b -> f [a]
separated  r s = many s *> delimited  r (many1 s) <* many s
separated1 r s = many s *> delimited1 r (many1 s) <* many s


look :: Rule t a -> Rule t a
look r = Rule $ \ts -> case step r ts of
    Accept a _ -> Accept a ts
    Reject _   -> Reject ts

match :: Eq t => t -> Rule t t
match t = Rule $ \case
    (t':ts) | t == t' -> Accept t' ts
    ts                -> Reject ts

matchIf :: (t -> Bool) -> Rule t t
matchIf p = Rule $ \case
    (t:ts) | p t -> Accept t ts
    ts           -> Reject ts

matchMaybe :: (t -> Maybe a) -> Rule t a
matchMaybe f = Rule $ \case
    (t:ts) | isJust (f t) -> Accept (fromJust (f t)) ts
    ts                    -> Reject ts


-- Unexpectable typeclass used for reporting errors
type Line = Int

class Unexpectable t where
    xline :: [t] -> Line
    xshow :: [t] -> String

    unexpected :: [t] -> a
    unexpected ts = error $ "unexpected " ++ case ts of
        [] -> "end of input"
        _  -> xshow ts ++ " on line " ++ show (xline ts + 1)

instance Unexpectable Char where
    xline = length . filter (== '\n')
    xshow = show . head


run :: (Unexpectable t) => Rule t a -> [t] -> a
run r ts = case step r ts of
    Accept a [] -> a
    Accept _ ts -> unexpected $ handled ts
    Reject   ts -> unexpected $ handled ts
  where
    handled ts' = reverse $ take (length ts - length ts' + 1) ts

