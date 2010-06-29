module Control.Concurrent.Speculation.Internal 
    ( Acc(..)
    , extractAcc
    , MaybeAcc(..)
    , fromMaybeAcc
    , errorEmptyStructure
    , returning
    ) where

import Data.Foldable
import Data.Traversable
import Control.Applicative

-- comonad!
data Acc a = Acc {-# UNPACK #-} !Int a

instance Functor Acc where
    fmap f (Acc n a) = Acc n (f a)

instance Foldable Acc where
    foldMap = foldMapDefault

instance Traversable Acc where
    traverse f (Acc n a) = Acc n <$> f a

extractAcc :: Acc a -> a
extractAcc (Acc _ a) = a
{-# INLINE extractAcc #-}

data MaybeAcc a = JustAcc {-# UNPACK #-} !Int a | NothingAcc

instance Functor MaybeAcc where
    fmap f (JustAcc n a) = JustAcc n (f a)
    fmap _ NothingAcc = NothingAcc

instance Foldable MaybeAcc where
    foldMap = foldMapDefault

instance Traversable MaybeAcc where
    traverse f (JustAcc n a) = JustAcc n <$> f a
    traverse _ NothingAcc    = pure NothingAcc

fromMaybeAcc :: a -> MaybeAcc a -> a
fromMaybeAcc _ (JustAcc _ a) = a
fromMaybeAcc a _ = a
{-# INLINE fromMaybeAcc #-}

errorEmptyStructure :: String -> a
errorEmptyStructure f = error $ f ++ ": error empty structure"

returning :: Monad m => (a -> b -> c) -> a -> b -> m c
returning f a b = return (f a b)
{-# INLINE returning #-}
