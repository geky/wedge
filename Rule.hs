module Rule where

import Prelude hiding (repeat)
import Control.Applicative


newtype Rule t a = Rule { step :: [t] -> Result t a }

data Result t a
    = Accept a [t]
    | Reject   [t]

infixl 2 `step`


instance Functor (Rule t) where
    fmap f r = Rule $ \t -> case step r t of
        Accept a t -> Accept (f a) t
        Reject   t -> Reject t

instance Applicative (Rule t) where
    pure a = Rule $ Accept a

    f <*> a = Rule $ \t -> case step f t of
        Accept f t -> f <$> a `step` t
        Reject   t -> Reject t

instance Alternative (Rule t) where
    empty = Rule Reject

    a <|> b = Rule $ \t -> case step a t of
        Accept a t -> Accept a t
        Reject   _ -> step b t

instance Monad (Rule t) where
    return a = Rule $ Accept a
    fail _   = Rule $ Reject

    a >>= f = Rule $ \t -> case step a t of
        Accept a t -> f a `step` t
        Reject   t -> Reject t


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


nomatch, match :: Eq a => a -> Rule a a
nomatch a = matchp (/= a)
match   a = matchp (== a)

nomatchp, matchp :: (a -> Bool) -> Rule a a
nomatchp p = matchp (not . p)
matchp   p = Rule $ \t -> case t of
    (t:ts) | p t -> Accept t ts
    ts           -> Reject ts


unexpected :: Show t => [t] -> a
unexpected (t:_) = error $ "unexpected " ++ (show t)
unexpected _     = error $ "unexpected end of input"

run :: Show t => Rule t a -> [t] -> a
run r t = case step r t of
  Accept a [] -> a
  Accept _ ts -> unexpected ts
  Reject   ts -> unexpected ts

