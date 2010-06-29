module Control.Concurrent.Speculation.Internal 
    ( Acc(..)
    , extractAcc
    , MaybeAcc(..)
    , fromMaybeAcc
    , errorEmptyStructure
    , returning
    ) where

data Acc a = Acc {-# UNPACK #-} !Int a

extractAcc :: Acc a -> a
extractAcc (Acc _ a) = a
{-# INLINE extractAcc #-}

data MaybeAcc a = JustAcc {-# UNPACK #-} !Int a | NothingAcc

fromMaybeAcc :: a -> MaybeAcc a -> a
fromMaybeAcc _ (JustAcc _ a) = a
fromMaybeAcc a _ = a
{-# INLINE fromMaybeAcc #-}

errorEmptyStructure :: String -> a
errorEmptyStructure f = error $ f ++ ": error empty structure"

returning :: Monad m => (a -> b -> c) -> a -> b -> m c
returning f a b = return (f a b)
{-# INLINE returning #-}
