{-# LANGUAGE Rank2Types #-}
module Control.Concurrent.Speculation.Internal 
    ( 
    -- * Determining if a closure is evaluated
      evaluated
    -- * Codensity monad
    , Codensity(..)
    , liftCodensity
    , lowerCodensity
--  , returning
    ) where

import Control.Applicative
import Control.Monad

import Data.Bits ((.&.))
import Foreign (sizeOf)
import Unsafe.Coerce (unsafeCoerce)


-- | Used to inspect tag bits
data Box a = Box a

-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that
-- relies on GHC internals and will falsely return 0, but (hopefully) never give the wrong tag number if it returns a non-0 value.
tag :: a -> Int
tag a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Int) - 1)
{-# INLINE tag #-}

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function
-- that relies on GHC internals and will return false negatives, but (hopefully) no false positives. This is unsafe as the value of this function will vary (from False to True) over the course of pure invocations!
evaluated :: a -> Bool
evaluated a = tag a /= 0
{-# INLINE evaluated #-}

-- returning :: Monad m => (a -> a -> b) -> a -> a -> m b
-- returning f a b = return (f a b)
-- {-# INLINE returning #-}

newtype Codensity f a = Codensity { runCodensity :: forall r. (a -> f r) -> f r } 

instance Functor (Codensity f) where
    fmap f (Codensity m) = Codensity $ \k -> m (k . f)

instance Applicative (Codensity f) where
    pure = return
    (<*>) = ap

instance Monad (Codensity f) where
    return x = Codensity (\k -> k x)
    Codensity m >>= f = Codensity (\k -> m (\a -> runCodensity (f a) k))

liftCodensity :: Monad m => m a -> Codensity m a 
liftCodensity m = Codensity (m >>=)

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity a = runCodensity a return
