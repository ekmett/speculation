{-# LANGUAGE MagicHash, Rank2Types, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Speculation.Traversable
-- Copyright   :  (C) 2010-2015 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (UnboxedTuples, Rank2Types)
--
----------------------------------------------------------------------------
module Control.Concurrent.Speculation.Traversable
    (
    -- * Traversable
    -- ** Applicative Traversals
      traverse, traverseBy
    , for, forBy
    , sequenceA, sequenceByA
    -- ** Monadic traversals
    , mapM, mapByM
    , sequence, sequenceBy
    , forM, forByM
    -- ** STM-based traversals with transactional rollback
    , mapSTM, mapBySTM
    , forSTM, forBySTM
    -- * Accumulating parameters
    , mapAccumL, mapAccumLBy
    , mapAccumR, mapAccumRBy
    ) where

import Prelude hiding
  ( mapM
  , sequence
#if __GLASGOW_HASKELL__ >= 710
  , traverse
  , sequenceA
#endif
  )
import GHC.Prim
import GHC.Types
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (Traversable)
#endif
import qualified Data.Traversable as Traversable
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Speculation
import Control.Concurrent.Speculation.Internal

mapAccumL :: (Traversable t, Eq a) => (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL = mapAccumLBy (==)
{-# INLINE mapAccumL #-}

mapAccumLBy :: Traversable t => (a -> a -> Bool) -> (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumLBy cmp g f z xs = runIntAccumL (Traversable.traverse go xs) 0 z
  where
    go b = IntAccumL (\n a ->
            let ~(a', c) = specBy cmp (g (I# n)) (`f` b) a
            in (# n +# 1#, a', c #))
{-# INLINE mapAccumLBy #-}

mapAccumR :: (Traversable t, Eq a) => (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR = mapAccumRBy (==)
{-# INLINE mapAccumR #-}

mapAccumRBy :: Traversable t => (a -> a -> Bool) -> (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumRBy cmp g f z xs = runIntAccumR (Traversable.traverse go xs) 0 z
  where
    go b = IntAccumR (\n a ->
            let ~(a', c) = specBy cmp (g (I# n)) (`f` b) a
            in (# n +# 1#, a', c #))
{-# INLINE mapAccumRBy #-}

traverse  :: (Traversable t, Applicative f, Eq a) => (Int -> a) -> (a -> f b) -> t a -> f (t b)
traverse = traverseBy (==)
{-# INLINE traverse #-}

traverseBy :: (Traversable t, Applicative f) => (a -> a -> Bool) -> (Int -> a) -> (a -> f b) -> t a -> f (t b)
traverseBy cmp g f xs = runAccT (Traversable.traverse go xs) 0
  where
    -- go :: a -> AccT f a
    go a = AccT $ \i -> acc (i +# 1#) $ specBy cmp (g (I# i)) f a
{-# INLINE traverseBy #-}

mapM :: (Traversable t, Monad m, Eq a) => (Int -> a) -> (a -> m b) -> t a -> m (t b)
mapM = mapByM (==)
{-# INLINE mapM #-}

mapByM :: (Traversable t, Monad m) => (a -> a -> Bool) -> (Int -> a) -> (a -> m b) -> t a -> m (t b)
mapByM cmp g f = unwrapMonad . traverseBy cmp g (WrapMonad . f)
{-# INLINE mapByM #-}

mapSTM :: (Traversable t, Eq a) => (Int -> STM a) -> (a -> STM b) -> t a -> STM (t b)
mapSTM = mapBySTM (returning (==))
{-# INLINE mapSTM #-}

mapBySTM :: Traversable t => (a -> a -> STM Bool) -> (Int -> STM a) -> (a -> STM b) -> t a -> STM (t b)
mapBySTM cmp g f xs = unwrapMonad (runAccT (Traversable.traverse go xs) 0)
  where
    go a = AccT $ \i -> acc (i +# 1#) $ WrapMonad $ specBySTM cmp (g (I# i)) f a
{-# INLINE mapBySTM #-}


sequenceA :: (Traversable t, Applicative f, Eq (f a)) => (Int -> f a) -> t (f a) -> f (t a)
sequenceA g = traverse g id
{-# INLINE sequenceA #-}

sequenceByA :: (Traversable t, Applicative f) => (f a -> f a -> Bool) -> (Int -> f a) -> t (f a) -> f (t a)
sequenceByA cmp g = traverseBy cmp g id
{-# INLINE sequenceByA #-}

sequence   :: (Traversable t, Monad m, Eq (m a)) => (Int -> m a) -> t (m a) -> m (t a)
sequence g = mapM g id
{-# INLINE sequence #-}

sequenceBy :: (Traversable t, Monad m) => (m a -> m a -> Bool) -> (Int -> m a) -> t (m a) -> m (t a)
sequenceBy cmp g = mapByM cmp g id
{-# INLINE sequenceBy #-}

{-
sequenceSTM   :: (Traversable t, Eq a) => (Int -> STM a) -> t (STM a) -> STM (t a)
sequenceSTM g = mapSTM g id
{-# INLINE sequenceSTM #-}

sequenceBySTM :: Traversable t => (a -> a -> STM Bool) -> (Int -> STM a) -> t (STM a) -> STM (t a)
sequenceBySTM cmp g = mapBySTM cmp g id
{-# INLINE sequenceBySTM #-}
-}

for :: (Traversable t, Applicative f, Eq a) => (Int -> a) -> t a -> (a -> f b) -> f (t b)
for g = flip (traverse g)
{-# INLINE for #-}

forBy :: (Traversable t, Applicative f) => (a -> a -> Bool) -> (Int -> a) -> t a -> (a -> f b) -> f (t b)
forBy cmp g = flip (traverseBy cmp g)
{-# INLINE forBy #-}

forM :: (Traversable t, Monad m, Eq a) => (Int -> a) -> t a -> (a -> m b) -> m (t b)
forM g = flip (mapM g)
{-# INLINE forM #-}

forByM :: (Traversable t, Monad m) => (a -> a -> Bool) -> (Int -> a) -> t a -> (a -> m b) -> m (t b)
forByM cmp g = flip (mapByM cmp g)
{-# INLINE forByM #-}

forSTM :: (Traversable t, Eq a) => (Int -> STM a) -> t a -> (a -> STM b) -> STM (t b)
forSTM g = flip (mapSTM g)
{-# INLINE forSTM #-}

forBySTM :: Traversable t => (a -> a -> STM Bool) -> (Int -> STM a) -> t a -> (a -> STM b) -> STM (t b)
forBySTM cmp g = flip (mapBySTM cmp g)
{-# INLINE forBySTM #-}

-- Utilities

acc :: Int# -> a -> Acc a
acc i a = Acc (I# i) a
{-# INLINE acc #-}

data IntAccumL s a = IntAccumL (Int# -> s -> (# Int#, s, a #))

runIntAccumL :: IntAccumL s a -> Int -> s -> (s, a)
runIntAccumL (IntAccumL m) (I# i) s = case m i s of
    (# _, s1, a #) -> (s1, a)
{-# INLINE runIntAccumL #-}

instance Functor (IntAccumL s) where
    fmap f (IntAccumL m) = IntAccumL  (\i s -> case m i s of
        (# i1, s1, a #) -> (# i1, s1, f a #))

instance Applicative (IntAccumL s) where
    pure a = IntAccumL (\i s -> (# i, s, a #))
    IntAccumL mf <*> IntAccumL ma = IntAccumL (\i s ->
        case mf i s of
            (# i1, s1, f #) ->
                case ma i1 s1 of
                    (# i2, s2, a #) -> (# i2, s2, f a #))

data IntAccumR s a = IntAccumR (Int# -> s -> (# Int#, s, a #))

runIntAccumR :: IntAccumR s a -> Int -> s -> (s, a)
runIntAccumR (IntAccumR m) (I# i) s = case m i s of
    (# _, s1, a #) -> (s1, a)
{-# INLINE runIntAccumR #-}

instance Functor (IntAccumR s) where
    fmap f (IntAccumR m) = IntAccumR  (\i s -> case m i s of
        (# i1, s1, a #) -> (# i1, s1, f a #))

instance Applicative (IntAccumR s) where
    pure a = IntAccumR (\i s -> (# i, s, a #))
    IntAccumR mf <*> IntAccumR ma = IntAccumR (\i s ->
        case ma i s of
            (# i1, s1, a #) ->
                case mf i1 s1 of
                    (# i2, s2, f #) -> (# i2, s2, f a #))

-- applicative composition with a strict integer state applicative
newtype AccT m a = AccT (Int# -> Acc (m a))

runAccT :: Applicative m => AccT m a -> Int -> m a
runAccT (AccT m) (I# i) = extractAcc (m i)
{-# INLINE runAccT #-}

instance Functor f => Functor (AccT f) where
    fmap f (AccT m) = AccT (\i# -> case m i# of Acc i a -> Acc i (fmap f a))

instance Applicative f => Applicative (AccT f) where
    pure a = AccT (\i -> Acc (I# i) (pure a))
    AccT mf <*> AccT ma = AccT (\i0# ->
        let !(Acc (I# i1#) f) = mf i0#
            (Acc i2 a) = ma i1#
        in  Acc i2 (f <*> a))
