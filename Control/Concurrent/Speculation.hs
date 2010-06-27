{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Speculation
    ( spec
    , evaluated
    , specFoldr
    , specFoldl
--    , specFoldl'
--    , specFoldr'
    , Speculative(..)
    ) where

import Prelude hiding (foldl, foldl1, foldr, foldr1)
import Data.Array
import Data.Ix ()
import Data.Foldable
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Control.Parallel (par)

import Data.Bits
import Foreign
import Unsafe.Coerce

data Box a = Box a

-- | Inspect the dynamic pointer tagging bits of a closure
tag :: a -> Int
tag a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Int) - 1)

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function
-- that relies on GHC internals and will return false negatives, but (hopefully) no false positives.
evaluated :: a -> Bool
evaluated a = tag a /= 0

-- | Evaluate a function using a cheap guess at the argument in parallel with forcing the argument.
--
-- This is one way to induce parallelism in an otherwise sequential task.
-- If the argument has already been evaluated, we avoid sparking the parallel computation.
spec :: Eq a => a -> (a -> b) -> a -> b
spec guess f a 
    | evaluated a = f a 
    | otherwise = 
        speculation `par` 
            if guess == a
            then speculation
            else f a
    where 
        speculation = f guess


specFoldr :: (Speculative f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
specFoldr = specFoldrN 0

specFoldl  :: (Speculative f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
specFoldl = specFoldlN 0

class Foldable f => Speculative f where
    -- | Compute a right biased fold. the estimator function is a guess at the value of the suffix
    specFoldr1 :: Eq a => (Int -> a) -> (a -> a -> a) -> f a -> a
    specFoldrN :: Eq b => Int -> (Int -> b) -> (a -> b -> b) -> b -> f a -> b

    -- | Compute a left biased fold. the estimator function is a guess at the value of the prefix
    specFoldl1 :: Eq a => (Int -> a) -> (a -> a -> a) -> f a -> a
    specFoldlN :: Eq b => Int -> (Int -> b) -> (b -> a -> b) -> b -> f a -> b

    specFoldr1 g f = specFoldr1 g f . toList
    specFoldrN n g f z = specFoldrN n g f z . toList

    specFoldl1 g f = specFoldl1 g f . toList
    specFoldlN n g f z = specFoldlN n g f z . toList

errorEmptyStructure :: String -> a
errorEmptyStructure f = error $ f ++ ": error empty structure"

instance Speculative [] where
    specFoldr1 g f = go 0  
      where
        go _ [] = errorEmptyStructure "specFoldr1"
        go !n (x:xs) = n' `seq` spec (g n') (f x) (go n' xs)
          where 
            n' = n + 1

    specFoldrN _ _ _ z [] = z
    specFoldrN !n g f z (x:xs) = n' `seq` spec (g n') (f x) (specFoldrN n' g f z xs)
      where 
        n' = n + 1
        
    specFoldl1 _ _ []     = errorEmptyStructure "specFoldl1"
    specFoldl1 g f (x:xs) = specFoldlN 1 g f x xs
    specFoldlN  _ _ _ z [] = z
    specFoldlN !n g f z (x:xs) = n' `seq` spec (g n') (\z' -> specFoldlN n' g f z' xs) (f z x)
      where 
        n' = n + 1

-- speculation never helps with at most one element
instance Speculative Maybe where
    specFoldr1 _   = foldr1
    specFoldrN _ _ = foldr
    specFoldl1 _   = foldl1
    specFoldlN _ _ = foldl

instance Ix i => Speculative (Array i)
instance Speculative Set
instance Speculative (Map a)
instance Speculative IntMap
instance Speculative Seq

-- specFoldl' :: (Speculative f, Eq a) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
-- specFoldl' g f z0 xs = specFoldr g' f' id xs z0 where
--     f' x k z = k $! f z x
--    g' = undefined -- n z = f (g n) z

-- specFoldr' :: (Speculative f, Eq a) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
-- specFoldr' g f z0 xs = specFoldl g' f' id xs z0 where
--    f' x k z = k $! f x z
--    g' = undefined -- n z = f (g n) z
