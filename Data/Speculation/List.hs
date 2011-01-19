{-# LANGUAGE BangPatterns #-}
module Data.Speculation.List
    ( 
    -- * Speculative scans
      scan, scanBy
    , scanMap, scanMapBy
    , scanr, scanrBy
    , scanl, scanlBy
    , scanr1, scanr1By
    , scanl1, scanl1By
    {-
    -- ** Speculative monadic scans
    , scanrM, scanrByM
    , scanlM, scanlByM
    -- * Speculative transactional monadic scans
    , scanrSTM, scanrBySTM
    , scanlSTM, scanlBySTM
    -}
    ) where


import Prelude hiding 
    (foldl, foldl1, foldr, foldr1
    , any, all, and, or, mapM_, sequence_
    , elem, notElem, sum, product
    , minimum, maximum, concat, concatMap
    , scanr, scanl, scanr1, scanl1
    )

import Data.Monoid
import qualified Data.List as List
import Data.Speculation
import Data.Speculation.Internal

-- | Given a valid estimator @g@, @'scan' g xs@ converts @xs@ into a list of the prefix sums.
-- 
-- @g n@ should supply an estimate of the value of the monoidal summation over the first @n@ elements of the container.
-- 
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the prefix sum, then this can
-- provide increased opportunities for parallelism.

scan :: (Monoid m, Eq m) => (Int -> m) -> [m] -> [m]
scan = scanBy (==)
{-# INLINE scan #-}

-- | 'scan' using 'specBy'
scanBy :: Monoid m => (m -> m -> Bool) -> (Int -> m) -> [m] -> [m]
scanBy cmp g = scanrBy cmp g mappend mempty
{-# INLINE scanBy #-}

-- | Given a valid estimator @g@, @'scanMap' g f xs@ converts @xs@ into a list of the prefix sums.
-- 
-- @g n@ should supply an estimate of the value of the monoidal summation over the first @n@ elements of the container.
-- 
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the scan, then this can
-- provide increased opportunities for parallelism.
--
-- > scan = scanMap id
-- > scanMap = scanMapBy (==)

scanMap :: (Monoid m, Eq m) => (Int -> m) -> (a -> m) -> [a] -> [m]
scanMap = scanMapBy (==)
{-# INLINE scanMap #-}

scanMapBy :: Monoid m => (m -> m -> Bool) -> (Int -> m) -> (a -> m) -> [a] -> [m]
scanMapBy cmp g f = scanrBy cmp g (mappend . f) mempty
{-# INLINE scanMapBy #-}

-- | Given a valid estimator @g@, @'scanr' g f z xs@ yields the same answer as @'scanr'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from scanning over the last @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the scan, then this can
-- provide increased opportunities for parallelism.

scanr :: Eq b => (Int -> b) -> (a -> b -> b) -> b -> [a] -> [b]
scanr = scanrBy (==)
{-# INLINE scanr #-}

scanrBy :: (b -> b -> Bool) -> (Int -> b) -> (a -> b -> b) -> b -> [a] -> [b]
scanrBy cmp g f z = map extractAcc . List.scanr mf (Acc 0 z)
  where 
    mf a (Acc n b) = let n' = n + 1 in Acc n' (specBy' cmp (g n') (f a) b)
{-# INLINE scanrBy #-}


scanl  :: Eq b => (Int -> b) -> (b -> a -> b) -> b -> [a] -> [b]
scanl = scanlBy (==) 
{-# INLINE scanl #-}

scanlBy  :: (b -> b -> Bool) -> (Int -> b) -> (b -> a -> b) -> b -> [a] -> [b]
scanlBy cmp g f z = map extractAcc . List.scanl mf (Acc 0 z)
  where
    mf (Acc n a) b = let n' = n + 1 in Acc n' (specBy' cmp (g n') (`f` b) a)
{-# INLINE scanlBy #-}

scanr1 :: Eq a => (Int -> a) -> (a -> a -> a) -> [a] -> [a]
scanr1 = scanr1By (==) 
{-# INLINE scanr1 #-}

scanr1By :: (a -> a -> Bool) -> (Int -> a) -> (a -> a -> a) -> [a] -> [a]
scanr1By cmp g f xs = map (fromMaybeAcc undefined) $ List.scanr mf NothingAcc xs
  where
    mf a (JustAcc n b) = let n' = n + 1 in JustAcc n' (specBy' cmp (g n') (f a) b)
    mf a NothingAcc = JustAcc 1 a
{-# INLINE scanr1By #-}

scanl1 :: Eq a => (Int -> a) -> (a -> a -> a) -> [a] -> [a]
scanl1 = scanl1By (==)
{-# INLINE scanl1 #-}

scanl1By :: (a -> a -> Bool) -> (Int -> a) -> (a -> a -> a) -> [a] -> [a]
scanl1By cmp g f xs = map (fromMaybeAcc undefined) $ List.scanl mf NothingAcc xs
  where
    mf (JustAcc n a) b = let n' = n + 1 in JustAcc n' (specBy' cmp (g n') (`f` b) a)
    mf NothingAcc b    = JustAcc 1 b
{-# INLINE scanl1By #-}

{-
scanlM :: (Monad m, Eq b) => (Int -> b) -> (b -> a -> m b) -> b -> [a] -> m [b]
scanlM = scanlByM (==)
{-# INLINE scanlM #-}

scanlByM ::  Monad m => (b -> b -> Bool) -> (Int -> b) -> (b -> a -> m b) -> b -> [a] -> m [b]
scanlByM  cmp g f mz = liftM (map extractAcc) . List.scanl go (liftM (map (Acc 0)) mz) 
  where
    go mia b = do
      Acc n a <- mia
      a' <- specBy' cmp (g n) (`f` b) a
      return (Acc (n + 1) a')
{-# INLINE scanlByM #-}

scanrM :: (Monad m, Eq (m b)) => (Int -> m b) -> (a -> b -> m b) -> m b -> [a] -> m [b]
scanrM = scanrByM (==)
{-# INLINE scanrM #-}

scanrByM :: Monad m => (m b -> m b -> Bool) -> (Int -> m b) -> (a -> b -> m b) -> m b -> [a] -> m [b]
scanrByM cmp g f mz = liftM (map extractAcc) . List.scanr go (liftM (map (Acc 0)) mz) 
  where
    go a mib = do
      Acc n b <- mib
      let !n' = n + 1
      b' <- specBy' cmp (g n') (>>= f a) (return b)
      return (Acc n' b')
{-# INLINE scanrByM #-}

scanlSTM :: Eq a => (Int -> STM a) -> (a -> b -> STM a) -> STM a -> [b] -> STM [a]
scanlSTM = scanlBySTM (returning (==))
{-# INLINE scanlSTM #-}

scanlBySTM :: (a -> a -> STM Bool) -> (Int -> STM a) -> (a -> b -> STM a) -> STM a -> [b] -> STM [a]
scanlBySTM cmp g f mz = liftM (map extractAcc) . List.scanl go (liftM (Acc 0) mz)
  where
    go mia b = do
      Acc n a <- mia
      let !n' = n + 1
      a' <- specBySTM' cmp (g n') (`f` b) a
      return (Acc n' a')
{-# INLINE scanlBySTM #-}

scanrSTM :: Eq b => (Int -> STM b) -> (a -> b -> STM b) -> STM b -> [a] -> STM [b]
scanrSTM = scanrBySTM (returning (==))
{-# INLINE scanrSTM #-}

scanrBySTM :: (b -> b -> STM Bool) -> (Int -> STM b) -> (a -> b -> STM b) -> STM b -> [a] -> STM [b]
scanrBySTM cmp g f mz = liftM (map extractAcc) . List.scanr go (liftM (Acc 0) mz)
  where
    go a mib = do
      Acc n b <- mib
      let !n' = n + 1
      b' <- specBySTM' cmp (g n') (f a) b
      return (Acc n' b')
{-# INLINE scanrBySTM #-}

-- | Given a valid estimator @g@, @'scanl' g f z xs@ yields the same answer as @'scanl'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from scaning over the first @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the scan, then this can
-- provide increased opportunities for parallelism.
-}
