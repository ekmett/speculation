{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Speculation
    ( spec
    , spec'
    , evaluated
    , specFoldr
    , specFoldl
    , specFoldr1
    , specFoldl1
    , specFoldrN
    , specFoldlN
    ) where

import Prelude hiding (foldl, foldl1, foldr, foldr1)
import Data.Ix ()
import Data.Foldable
import Control.Parallel (par)

import Data.Bits
import Foreign
import Unsafe.Coerce

data Box a = Box a

-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that
-- relies on GHC internals and will falsely return 0, but (hopefully) never give the wrong tag number if it returns 
-- a non-0 value.
tag :: a -> Int
tag a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Int) - 1)
{-# INLINE tag #-}

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function
-- that relies on GHC internals and will return false negatives, but (hopefully) no false positives.
evaluated :: a -> Bool
evaluated a = tag a /= 0
{-# INLINE evaluated #-}

-- | @'spec' g f a@ evaluates @f g@ while forcing @a@, if @g == a@ then @f g@ is returned. Otherwise @f a@ is evaluated.
--
-- Furthermore, if the argument has already been evaluated, we avoid sparking the parallel computation at all.
--
-- If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task. 
--
-- However, if the guess isn\'t available more cheaply than the actual answer, then this saves no work and if the guess is
-- wrong, you risk evaluating the function twice.
--
-- > spec a f a = f $! a
--
-- The best-case timeline looks like:
--
-- > [---- f g ----]
-- >    [----- a -----]
-- > [-- spec g f a --]
-- 
-- The worst-case timeline looks like:
--
-- > [---- f g ----]
-- >    [----- a -----]
-- >                  [---- f a ----]
-- > [------- spec g f a -----------]
--
-- Compare these to the timeline of @f $! a@:
--
-- > [---- a -----]
-- >              [---- f a ----]

spec :: Eq a => a -> (a -> b) -> a -> b
spec g f a 
    | evaluated a = f a 
    | otherwise = spec' g f a
{-# INLINE spec #-}

-- | Unlike 'spec', this version does not check to see if the argument has already been evaluated. This can save
-- a small amount of work when you know the argument will always require computation.

spec' :: Eq a => a -> (a -> b) -> a -> b
spec' guess f a = 
    speculation `par` 
        if guess == a
        then speculation
        else f a
    where 
        speculation = f guess
{-# INLINE spec' #-}

-- | Given a valid estimator @g@, @'specFoldr' g f z xs@ yields the same answer as @'foldr'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from folding over the last @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.
--
-- > specFoldr = specFoldrN 0

specFoldr :: (Foldable f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
specFoldr = specFoldrN 0
{-# INLINE specFoldr #-}

-- | Given a valid estimator @g@, @'specFoldl' g f z xs@ yields the same answer as @'foldl'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from folding over the first @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.
--
-- > specFoldl = specFoldlN 0

specFoldl  :: (Foldable f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
specFoldl = specFoldlN 0
{-# INLINE specFoldl #-}

-- | 'specFoldr1' is to 'foldr1'' as 'specFoldr' is to 'foldr''
specFoldr1 :: (Foldable f, Eq a) => (Int -> a) -> (a -> a -> a) -> f a -> a
specFoldr1 g f = specFoldr1List g f . toList
{-# INLINE specFoldr1 #-}

specFoldr1List :: Eq a => (Int -> a) -> (a -> a -> a) -> [a] -> a
specFoldr1List g f = go 0  
      where
        go _ []  = errorEmptyStructure "specFoldr1"
        go _ [x] = x
        go !n (x:xs) = n' `seq` spec' (g n') (f x) (go n' xs)
          where 
            n' = n + 1
{-# INLINE specFoldr1List #-}

-- | Given a valid estimator @g@, @'specFoldrN' n g f z xs@ yields the same answer as @'foldr' f z xs@.
-- 
-- @g m@ should supply an estimate of the value returned from folding over the last @m - n@ elements of the container.
specFoldrN :: (Foldable f, Eq b) => Int -> (Int -> b) -> (a -> b -> b) -> b -> f a -> b
specFoldrN n0 g f z = go n0 . toList
    where
        go _ [] = z
        go !n (x:xs) = n' `seq` spec' (g n') (f x) (go n' xs)
          where 
            n' = n + 1
{-# INLINE specFoldrN #-}

-- | 'specFoldl1' is to 'foldl1'' as 'specFoldl' is to 'foldl''
specFoldl1 :: (Foldable f, Eq a) => (Int -> a) -> (a -> a -> a) -> f a -> a
specFoldl1 g f = specFoldl1List g f . toList
{-# INLINE specFoldl1 #-}

specFoldl1List :: Eq a => (Int -> a) -> (a -> a -> a) -> [a] -> a
specFoldl1List _ _ []     = errorEmptyStructure "specFoldl1"
specFoldl1List g f (x:xs) = specFoldlN 1 g f x xs
{-# INLINE specFoldl1List #-}

-- | Given a valid estimator @g@, @'specFoldlN' n g f z xs@ yields the same answer as @'foldl' f z xs@.
-- 
-- @g m@ should supply an estimate of the value returned from folding over the first @m - n@ elements of the container.
specFoldlN :: (Foldable f, Eq b) => Int -> (Int -> b) -> (b -> a -> b) -> b -> f a -> b
specFoldlN n0 g f z0 = go n0 z0 . toList
  where
    go _ z [] = z
    go !n z (x:xs) = n' `seq` spec' (g n') (\z' -> go n' z' xs) (f z x)
      where 
        n' = n + 1
{-# INLINE specFoldlN #-}

errorEmptyStructure :: String -> a
errorEmptyStructure f = error $ f ++ ": error empty structure"
