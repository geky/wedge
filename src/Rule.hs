

module Rule (many, optional, module Rule) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Bifunctor


-- Rule definition and operations
newtype Rule s t a = Rule
    { unrule :: forall b . 
        ( (s,a) -> [(s,t)] -> b -- accept
        ,          [(s,t)] -> b -- reject
        ,          [(s,t)] -> b -- fail
        ) -> [(s,t)] -> b   -- rule
    }

rule :: ([t] -> Rule s t a) -> Rule s t a
rule x = Rule $ \c ts -> unrule (x $ map snd ts) c ts

accept :: Int -> a -> Rule s t a
accept n x = Rule $ \(a,_,_) ts -> a (second (const x) $ head ts) (drop n ts)

reject :: Rule s t a
reject = Rule $ \(_,r,_) ts -> r ts


instance Functor (Rule s t) where
    fmap f x = Rule $ \(a,r,e) -> unrule x (a . second f,r,e)

instance Applicative (Rule s t) where
    pure = accept 0
    x <*> y = Rule $ \(a,r,e) -> unrule x 
        (\(s,f) -> unrule y (a . bimap (const s) f,e,e),r,e)

instance Monad (Rule s t) where
    return = pure
    fail _ = empty
    x >>= y = Rule $ \(a,r,e) -> unrule x
        (\(s,z) -> unrule (y z) (a . first (const s),e,e),r,e)

instance Alternative (Rule s t) where
    empty = reject
    x <|> y = Rule $ \(a,r,e) -> unrule x (a,unrule y (a,r,e),e)

instance MonadPlus (Rule s t) where
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
look :: Rule s t a -> Rule s t a
look x = Rule $ \c ts -> unrule x (new c ts) ts
  where new (a,r,_) ts = (\z _ -> a z ts, \_ -> r ts, \_ -> r ts)

try :: Rule s t a -> Rule s t a
try x = Rule $ \(a,r,_) ts -> unrule x (a, \_ -> r ts, \_ -> r ts) ts

at :: Rule s t a -> Rule s t (s, a)
at x = Rule $ \(a,r,e) ts -> unrule x (\(s,z) -> a (s,(s,z)), r, e) ts


-- general rules
current :: Rule s t t
current = look matchAny

match :: Eq t => t -> Rule s t t
match t = matchIf (== t)

matches :: Eq t => [t] -> Rule s t [t]
matches ts = rule $ \case
    ts' | isPrefixOf ts ts' -> accept (length ts) ts
    _                       -> reject

matchIf :: (t -> Bool) -> Rule s t t
matchIf p = rule $ \case
    t:_ | p t -> accept 1 t
    _         -> reject

matchAny :: Rule s t t
matchAny = matchIf $ const True

matchMaybe :: (t -> Maybe a) -> Rule s t a
matchMaybe f = fromJust . f <$> matchIf (isJust . f)


-- Running the actual rules
run :: Rule s t a -> [(s,t)] -> Either [(s,t)] a
run x = unrule x (end, Left, Left)
  where
    end (_, a) [] = Right a
    end _ ts      = Left ts

run1 :: Rule s t a -> [t] -> Either [t] a
run1 x = first (map snd) . run x . zip (repeat undefined)

