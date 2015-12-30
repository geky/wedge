module Rule where

import Control.Applicative
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
look :: Rule t a -> Rule t a
look x = Rule $ \c ts -> unrule x (c' c ts) ts
  where c' (a,r,_) ts = (\z _ -> a z ts, \_ -> r ts, \_ -> r ts)

try :: Rule t a -> Rule t a
try x = Rule $ \(a,r,_) -> unrule x (a,r,r)

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
runEither :: Rule t a -> [t] -> Either [t] a
runEither r ts = unrule r (end, Left, Left) ts
  where
    end a [] = Right a
    end _ ts = Left ts


type Line = Int

unexpected :: Show t => [t] -> [Line] -> a
unexpected ts ls = error $ "unexpected " ++ case (ts, ls) of
    (t:_, l:_) -> show t ++ " on line " ++ show (l + 1)
    _          -> "end of input"

run :: Show t => Rule t a -> [t] -> [Line] -> a
run r ts ls = case runEither r ts of
    Left ts' -> unexpected ts' $ drop (length ts-length ts'+1) ls
    Right a  -> a

