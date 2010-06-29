{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable #-}
module Control.Concurrent.Speculation
    ( 
    -- * Speculative application
      spec
    , spec'
    , specBy
    , specBy'
    , specOn
    , specOn'
    -- * Speculative application with transactional rollback
    , specSTM
    , specSTM'
    , specOnSTM
    , specOnSTM'
    , specBySTM
    , specBySTM'
    -- * Determining if a closure is evaluated
    , unsafeGetTagBits
    , unsafeIsEvaluated
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Speculation.Internal (returning)
import Control.Parallel (par)
import Control.Monad (liftM2, unless)
import Data.Function (on)

#if __GLASGOW_HASKELL__ >= 608
import Data.Bits ((.&.))
import Foreign (sizeOf)
import Unsafe.Coerce (unsafeCoerce)
-- dynamic pointer tagging is present on this platform
#define TAGGED 
#endif 

-- * Basic speculation

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
specBy cmp guess f a
    | unsafeIsEvaluated a = f a
    | otherwise = specBy' cmp guess f a
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

-- * STM-based speculation

-- | @'specSTM' g f a@ evaluates @f g@ while forcing @a@, if @g == a@ then @f g@ is returned. Otherwise the side-effects 
-- of the current STM transaction are rolled back and @f a@ is evaluated.
--   
-- If the argument @a@ is already evaluated, we don\'t bother to perform @f g@ at all.
--
-- If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task. 
--
-- However, if the guess isn\'t available more cheaply than the actual answer then this saves no work, and if the guess is
-- wrong, you risk evaluating the function twice.
--
-- > specSTM a f a = f $! a
--
-- The best-case timeline looks like:
--
-- > [------ f g ------]
-- >     [------- a -------]
-- > [--- specSTM g f a ---]
--
-- The worst-case timeline looks like:
--
-- > [------ f g ------] 
-- >     [------- a -------]
-- >                       [-- rollback --]
-- >                                      [------ f a ------]     
-- > [------------------ spec g f a ------------------------]
--
-- Compare these to the timeline of @f $! a@:
--
-- > [------- a -------]
-- >                   [------ f a ------]

specSTM :: Eq a => STM a -> (a -> STM b) -> a -> STM b
specSTM = specBySTM (returning (==))
{-# INLINE specSTM #-}

-- | Unlike 'specSTM', 'specSTM'' doesn't check if the argument has already been evaluated.

specSTM' :: Eq a => STM a -> (a -> STM b) -> a -> STM b
specSTM' = specBySTM' (returning (==))
{-# INLINE specSTM' #-}

-- | 'specSTM' using a user defined comparison function
specBySTM :: (a -> a -> STM Bool) -> STM a -> (a -> STM b) -> a -> STM b
specBySTM cmp guess f a 
    | unsafeIsEvaluated a = f a 
    | otherwise   = specBySTM' cmp guess f a
{-# INLINE specBySTM #-}

-- | 'specSTM'' using a user defined comparison function
specBySTM' :: (a -> a -> STM Bool) -> STM a -> (a -> STM b) -> a -> STM b
specBySTM' cmp mguess f a = a `par` do
    guess <- mguess
    result <- f guess
    -- rendezvous with a
    matching <- cmp guess a 
    unless matching retry 
    return result
  `orElse` 
    f a 
{-# INLINE specBySTM' #-}

-- | @'specBySTM' . 'on' (==)@
specOnSTM :: Eq c => (a -> STM c) -> STM a -> (a -> STM b) -> a -> STM b
specOnSTM = specBySTM . on (liftM2 (==))
{-# INLINE specOnSTM #-}

-- | @'specBySTM'' . 'on' (==)@
specOnSTM' :: Eq c => (a -> STM c) -> STM a -> (a -> STM b) -> a -> STM b
specOnSTM' = specBySTM' . on (liftM2 (==))
{-# INLINE specOnSTM' #-}


-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that relies on GHC internals and may falsely return 0, but never give the wrong tag number if it returns a non-0 value.
unsafeGetTagBits :: a -> Int
{-# INLINE unsafeGetTagBits #-}
#ifndef TAGGED
unsafeGetTagBits _ = 0
#else
unsafeGetTagBits a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Int) - 1)
-- | Used to inspect tag bits
data Box a = Box a
#endif

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function that relies on GHC internals and will return false negatives, but no false positives. This is unsafe as the value of this function will vary (from False to True) over the course of otherwise pure invocations!
unsafeIsEvaluated :: a -> Bool
unsafeIsEvaluated a = unsafeGetTagBits a /= 0
{-# INLINE unsafeIsEvaluated #-}

