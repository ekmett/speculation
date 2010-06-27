{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Speculation
    ( 
    -- * Speculative application
      spec
    , spec'
    , specBy
    , specBy'
    , specOn
    , specOn'
    -- * Detecting closure evaluation
    , evaluated
    ) where

import Control.Parallel (par)
import Data.Function (on)
import Data.Bits ((.&.))
import Foreign (sizeOf)
import Unsafe.Coerce (unsafeCoerce)

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
spec = specBy (==) 
{-# INLINE spec #-}

-- | Unlike 'spec', this version does not check to see if the argument has already been evaluated. This can save
-- a small amount of work when you know the argument will always require computation.

spec' :: Eq a => a -> (a -> b) -> a -> b
spec' = specBy' (==)
{-# INLINE spec' #-}

-- | 'spec' with a user defined comparison function
specBy :: (a -> a -> Bool) -> a -> (a -> b) -> a -> b
specBy cmp g f a
    | evaluated a = f a
    | otherwise = specBy' cmp g f a
{-# INLINE specBy #-}

-- | 'spec'' with a user defined comparison function
specBy' :: (a -> a -> Bool) -> a -> (a -> b) -> a -> b
specBy' cmp guess f a = 
    speculation `par` 
        if cmp guess a
        then speculation
        else f a
    where 
        speculation = f guess
{-# INLINE specBy' #-}

-- | 'spec' comparing by projection onto another type
specOn :: Eq c => (a -> c) -> a -> (a -> b) -> a -> b
specOn = specBy . on (==)
{-# INLINE specOn #-}

-- | 'spec'' comparing by projection onto another type
specOn' :: Eq c => (a -> c) -> a -> (a -> b) -> a -> b
specOn' = specBy' . on (==)
{-# INLINE specOn' #-}
