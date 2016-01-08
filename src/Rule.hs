module Rule (many, optional, module Rule) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List


-- Rule definition and operations
newtype Rule t a = Rule
    { unrule :: forall b .
        ( a -> [t] -> b -- accept
        ,      [t] -> b -- reject
        ,      [t] -> b -- fail
        ) -> [t] -> b   -- rule
    }

rule :: ([t] -> Rule t a) -> Rule t a
rule x = Rule $ \c ts -> unrule (x ts) c ts

accept :: a -> [t] -> Rule t a
accept x ts = Rule $ \(a,_,_) _ -> a x ts

reject :: Rule t a
reject = Rule $ \(_,r,_) -> r


instance Functor (Rule t) where
    fmap f x = Rule $ \(a,r,e) -> unrule x (a.f,r,e)

instance Applicative (Rule t) where
    pure = rule . accept
    x <*> y = Rule $ \(a,r,e) -> unrule x (\f -> unrule y (a.f,e,e), r, e)

instance Alternative (Rule t) where
    empty = reject
    x <|> y = Rule $ \(a,r,e) -> unrule x (a, unrule y (a,r,e), e)

instance Monad (Rule t) where
    return = pure
    fail _ = empty
    x >>= y = Rule $ \(a,r,e) -> unrule x (\z -> unrule (y z) (a,e,e), r, e)

instance MonadPlus (Rule t) where
    mzero = empty
    mplus = (<|>)


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
separated  r s = many s *> ((:) <$> r <*> rest <|> pure [])
  where rest = s *> separated r s <|> pure []
separated1 r s = many s *> ((:) <$> r <*> rest)
  where rest = s *> separated r s <|> pure []

suffix, suffix1 :: Alternative f => f a -> f (a -> a) -> f a
suffix  r s = r <**> (foldl (flip (.)) id <$> many s)
suffix1 r s = suffix (r <**> s) s

prefix, prefix1 :: Alternative f => f (a -> a) -> f a -> f a
prefix  p r = (foldr (.) id <$> many p) <*> r
prefix1 p r = prefix p (p <*> r)


-- rule modifiers
look :: Rule t a -> Rule t a
look x = Rule $ \c ts -> unrule x (new c ts) ts
  where new (a,r,_) ts = (\z _ -> a z ts, \_ -> r ts, \_ -> r ts)

try :: Rule t a -> Rule t a
try x = Rule $ \(a,r,_) ts -> unrule x (a, \_ -> r ts, \_ -> r ts) ts

over :: (t0 -> t1) -> Rule t1 a -> Rule t0 a
over f x = Rule $ \c ts -> unrule x (new c ts) (map f ts)
  where
    new (a,r,e) ts = (\z -> a z . up ts, r . up ts, e . up ts)
    up ts ts' = drop (length ts - length ts') ts

propagate :: (t -> a) -> Rule t (a -> b) -> Rule t b
propagate f x = rule $ \case
    t:_ -> ($ f t) <$> x
    _   -> reject


-- general rules
current :: Rule t t
current = look matchAny

match :: Eq t => t -> Rule t t
match t = matchIf (== t)

matches :: Eq t => [t] -> Rule t [t]
matches ts = rule $ \case
    ts' | isPrefixOf ts ts' -> uncurry accept $ splitAt (length ts) ts'
    _                       -> reject

matchIf :: (t -> Bool) -> Rule t t
matchIf p = rule $ \case
    t:ts | p t -> accept t ts
    _          -> reject

matchAny :: Rule t t
matchAny = matchIf $ const True

matchMaybe :: (t -> Maybe a) -> Rule t a
matchMaybe f = fromJust . f <$> matchIf (isJust . f)


-- Running the actual rules
run :: Rule t a -> ([t] -> a) -> [t] -> a
run r fail = unrule r (end, fail, fail)
  where
    end a [] = a
    end _ ts = fail ts

