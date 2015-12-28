module Rule where

import Control.Applicative
import Data.Maybe


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
reject = Rule $ \(_,r,_) ts -> r ts


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


-- miscellaneous rules
current :: Rule t t
current = look matchAny

look :: Rule t a -> Rule t a
look x = Rule $ \c ts -> unrule x (c' c ts) ts
  where c' (a,r,_) ts = (\z _ -> a z ts, \_ -> r ts, \_ -> r ts)

matchAny :: Rule t t
matchAny = matchIf $ const True

match :: Eq t => t -> Rule t t
match t = matchIf (== t)

matches :: Eq t => [t] -> Rule t [t]
matches = foldr (liftA2 (:)) (pure []) . map match

matchIf :: (t -> Bool) -> Rule t t
matchIf p = rule $ \case
    t:ts | p t -> accept t ts
    _          -> reject

matchMaybe :: (t -> Maybe a) -> Rule t a
matchMaybe f = fromJust . f <$> matchIf (isJust . f)


-- Unexpectable typeclass used for reporting errors
type Line = Int

class Unexpectable t where
    xline :: [t] -> Line
    xshow :: [t] -> String

    xlines :: [t] -> [[t]]
    xlines = xlines' 0 []
      where
        xlines' :: Unexpectable t => Line -> [t] -> [t] -> [[t]]
        xlines' _ _ []                         = [[]]
        xlines' c r ts@(t:_) | xline (t:r) > c = [] : xlines' (c+1) r ts
        xlines' c r (t:ts)                     = (t:l) : ls
          where l:ls = xlines' c (t:r) ts

    unexpected :: [t] -> a
    unexpected ts = error $ "unexpected " ++ case ts of
        [] -> "end of input"
        _  -> xshow ts ++ " on line " ++ show (xline ts + 1)

instance Unexpectable Char where
    xline = length . filter (== '\n')
    xshow = show . head


run :: Unexpectable t => Rule t a -> [t] -> a
run r ts = unrule r (end, unexpect, unexpect) ts
  where
    end a [] = a
    end _ ts = unexpect ts
    unexpect ts' = unexpected $ reverse $ take (length ts-length ts'+1) ts

