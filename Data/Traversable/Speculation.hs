{-# LANGUAGE MagicHash, Rank2Types, UnboxedTuples #-}
module Data.Traversable.Speculation
    ( mapAccumL, mapAccumLBy
    , mapAccumR, mapAccumRBy
    ) where

import GHC.Prim
import GHC.Types
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Control.Applicative
import Control.Concurrent.Speculation

data IntAccumL s a = IntAccumL (Int# -> s -> (# Int#, s, a #))

runIntAccumL :: IntAccumL s a -> Int -> s -> (s, a)
runIntAccumL (IntAccumL m) (I# i) s = case m i s of
    (# _, s1, a #) -> (s1, a)

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
    
mapAccumL :: (Traversable t, Eq a, Eq c) => (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL = mapAccumLBy (==)

mapAccumLBy :: Traversable t => (a -> a -> Bool) -> (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumLBy cmp g f z xs = runIntAccumL (Traversable.traverse go xs) 0 z
  where
    go b = IntAccumL (\n a -> 
            let ~(a', c) = specBy' cmp (g (I# n)) (`f` b) a
            in (# n +# 1#, a', c #))

data IntAccumR s a = IntAccumR (Int# -> s -> (# Int#, s, a #))

runIntAccumR :: IntAccumR s a -> Int -> s -> (s, a)
runIntAccumR (IntAccumR m) (I# i) s = case m i s of
    (# _, s1, a #) -> (s1, a)

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

mapAccumR :: (Traversable t, Eq a, Eq c) => (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR = mapAccumRBy (==)

mapAccumRBy :: Traversable t => (a -> a -> Bool) -> (Int -> a) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumRBy cmp g f z xs = runIntAccumR (Traversable.traverse go xs) 0 z
  where
    go b = IntAccumR (\n a -> 
            let ~(a', c) = specBy' cmp (g (I# n)) (`f` b) a
            in (# n +# 1#, a', c #))

{-
traverse :: (Traversable t, Applicative f, Eq (f b)) => (Int -> f b) -> (a -> f b) -> t a -> f (t b)
traverse = traverseBy (==)

traverseBy :: (Traversable t, Applicative f) => (Int -> f b) -> (a -> f b) -> t a -> f (t b)
-}

-- note applicative composition doesn't give StateT
-- There is a difference between StateT s m and State s (m a)
-- 
{-
newtype AccT m a = AccT (Int# -> m (Acc a))

instance Functor f => Applicative (AccT s f) where
    fmap f (AccT m) = AccT (fmap (fmap f) . m)

instance Applicative m => Applicative (AccT s m) where
    pure a = AccT (\i -> return (Acc i a))
    AccT mf <*> AccT ma = AccT (\i -> 
        let maccf = mf i 
    

        m (Acc (a -> b)) -> m (Acc a)
-}

{-
traverseBy :: (Traversable t, Applicative f) => (f b -> f b -> Bool) -> (Int -> f b) -> (a -> f b) -> t a -> f (t b)
sequence   :: (Traversable t, Monad m, Eq (m a)) => (Int -> m a) -> t (m a) -> m (t a)
sequenceBy :: (Traversable t, Monad m) => (m a -> m a -> Bool) -> (Int -> m a) -> t (m a) -> m (t a)
sequenceA   :: (Traversable t, Applicative f, Eq (f a)) => (Int -> f a) -> t (f a) -> f (t a)
sequenceByA :: (Traversable t, Applicative f) => (f a -> f a -> Bool) -> (Int -> f a) -> t (f a) -> f (t a)
sequenceSTM   :: (Traversable t, Eq a) => (Int -> STM a) -> t (STM a) -> STM (t a)
sequenceBySTM :: Traversable t => (a -> a -> STM Bool) -> (Int -> STM a) -> t (STM a) -> STM (t a)
mapM :: (Traversable t, Monad m, Eq (m b)) => (Int -> m b) -> (a -> m b) -> t a -> m (t b)
mapByM :: (Traversable t, Monad m) => (m b -> m b -> Bool) -> (Int -> m b) -> (a -> m b) -> t a -> m (t b)
mapSTM :: (Traversable t, Eq b) => (Int -> STM b) -> (a -> STM b) -> t a -> STM (t b)
mapBySTM :: Traversable t => (b -> b -> STM Bool) -> (Int -> STM b) -> (a -> STM b) -> t a -> STM (t b)
mapAccumR :: (Traversable t, Eq a, Eq c) => (Int -> (a, c)) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumRBy :: Traversable t => ((a, c) -> (a, c) -> Bool) -> (Int -> (a, c)) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumLBy :: Traversable t => ((a, c) -> (a, c) -> Bool) -> (Int -> (a, c)) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
for :: (Traversable t, Applicative f, Eq (f b)) => (Int -> f b) -> t a -> (a -> f b) -> f (t b)
forBy :: (Traversable t, Applicative f) => (f b -> f b -> Bool) -> (Int -> f b) -> t a -> (a -> f b) -> f (t b)
forM :: (Traversable t, Monad m, Eq (m b)) => (Int -> m b) -> t a -> (a -> m b) -> m (t b)
forByM :: (Traversable t, Monad m) => (m b -> m b -> Bool) -> (Int -> m b) -> t a -> (a -> m b) -> m (t b)
forSTM :: (Traversable t, Eq b) => (Int -> STM b) -> t a -> (a -> STM b) -> STM (t b)
forBySTM :: Traversable t => (b -> b -> STM Bool) -> (Int -> STM b) -> t a -> (a -> STM b) -> STM (t b)
-}

