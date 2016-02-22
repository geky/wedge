module Nested where

import Control.Applicative
import Control.Monad


-- Nested type for wrapping nested monads
newtype Nested m1 m2 a = Nested (m1 (m2 a))


-- Lifting single monads
nest :: m1 (m2 a) -> Nested m1 m2 a
nest = Nested

unnest :: Nested m1 m2 a -> m1 (m2 a)
unnest (Nested a) = a

lift1 :: (Functor m1, Applicative m2) => m1 a -> Nested m1 m2 a
lift1 = Nested . fmap pure

lift2 :: (Applicative m1) => m2 a -> Nested m1 m2 a
lift2 = Nested . pure


-- Instance declarations
instance (Functor m1, Functor m2) => Functor (Nested m1 m2) where
    fmap f = Nested . fmap (fmap f) . unnest

instance (Foldable m1, Foldable m2) => Foldable (Nested m1 m2) where
    foldMap f = foldMap (foldMap f) . unnest

instance (Traversable m1, Traversable m2) => Traversable (Nested m1 m2) where
    traverse f = fmap Nested . traverse (traverse f) . unnest

instance (Applicative m1, Applicative m2) => Applicative (Nested m1 m2) where
    pure = Nested . pure . pure
    Nested x <*> Nested y = Nested $ liftA2 (<*>) x y

instance (Applicative m1, Alternative m2) => Alternative (Nested m1 m2) where
    empty = Nested $ pure empty
    Nested x <|> Nested y = Nested $ liftA2 (<|>) x y

instance (Monad m1, Monad m2, Traversable m2) => Monad (Nested m1 m2) where
    return = pure
    fail = Nested . fail
    x >>= y = Nested $ (liftM.liftM) (unnest . y) (unnest x)
        >>= liftM join . sequence

