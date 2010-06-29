module Data.Traversable.Speculation
    ( 
    ) where

{-
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Control.Concurrent.STM
import Control.Concurrent.Speculation
import Control.Concurrent.Speculation.Internal
import Control.Applicative
import Control.Monad hiding (mapM_, msum, forM_, sequence_)

traverse :: (Traversable t, Applicative f, Eq (f b)) => (Int -> f b) -> (a -> f b) -> t a -> f (t b)
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
mapAccumL :: (Traversable t, Eq a, Eq c) => (Int -> (a, c)) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumLBy :: Traversable t => ((a, c) -> (a, c) -> Bool) -> (Int -> (a, c)) -> (a -> b -> (a, c)) -> a -> t b -> (a, t c)
for :: (Traversable t, Applicative f, Eq (f b)) => (Int -> f b) -> t a -> (a -> f b) -> f (t b)
forBy :: (Traversable t, Applicative f) => (f b -> f b -> Bool) -> (Int -> f b) -> t a -> (a -> f b) -> f (t b)
forM :: (Traversable t, Monad m, Eq (m b)) => (Int -> m b) -> t a -> (a -> m b) -> m (t b)
forByM :: (Traversable t, Monad m) => (m b -> m b -> Bool) -> (Int -> m b) -> t a -> (a -> m b) -> m (t b)
forSTM :: (Traversable t, Eq b) => (Int -> STM b) -> t a -> (a -> STM b) -> STM (t b)
forBySTM :: Traversable t => (b -> b -> STM Bool) -> (Int -> STM b) -> t a -> (a -> STM b) -> STM (t b)
-}
