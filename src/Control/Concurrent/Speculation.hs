{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Speculation
-- Copyright   :  (C) 2008-2015 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Concurrent.Speculation
    (
    -- * Speculative application
      spec
    , specBy
    , specOn
    -- * Speculative application with transactional rollback
    , specSTM
    , specOnSTM
    , specBySTM
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Speculation.Internal (returning)
import Control.Monad (liftM2, unless)
import Data.Function (on)
import GHC.Conc

-- * Basic speculation

-- | @'spec' g f a@ evaluates @f g@ while forcing @a@, if @g == a@ then @f g@ is returned, otherwise @f a@ is evaluated and returned. Furthermore, if the argument has already been evaluated or are not running on the threaded runtime, we skip the @f g@ computation entirely. If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task. However, if the guess isn\'t available more cheaply than the actual answer, then this saves no work and if the guess is wrong, you risk evaluating the function twice. Under high load or in a runtime with access to a single capability, since 'f g' is computed via the spark queue, the speculation will be skipped and you will obtain the same answer as 'f $! a'.
--
--The best-case timeline looks like:
--
-- > foreground: [----- a -----]
-- > foreground:               [-]    (check g == a)
-- > spark:         [----- f g -----]
-- > overall:    [--- spec g f a ---]
--
-- The worst-case timeline looks like:
--
-- > foreground: [----- a -----]
-- > foreground:               [-]               (check g == a)
-- > foreground:                 [---- f a ----]
-- > spark:         [----- f g -----]
-- > overall:    [-------- spec g f a ---------]
--
-- Note that, if @f g@ takes longer than a to compute, in the HEAD release of GHC, @f g@ will be collected and killed during garbage collection.
--
-- > foreground: [----- a -----]
-- > foreground:               [-]               (check g == a)
-- > foreground:                 [---- f a ----]
-- > spark:         [---- f g ----######         (#'s mark when this spark is collectable)
-- > overall:    [--------- spec g f a --------]
-- 
-- Under high load:
--
-- > foreground: [----- a -----]
-- > foreground:               [-]               (check g == a)
-- > foreground:                 [---- f a ----]
-- > overall:    [-------- spec g f a ---------]
--
-- Compare these to the timeline of @f $! a@:
--
-- > foreground: [----- a -----]
-- > foreground:               [---- f a ----]
-- > orverall:   [---------- f $! a ---------]

spec :: Eq a => a -> (a -> b) -> a -> b
spec = specBy (==)
{-# INLINE spec #-}

-- | 'spec' with a user defined comparison function
specBy :: (a -> a -> Bool) -> a -> (a -> b) -> a -> b
specBy cmp guess f a
  | numCapabilities == 1 = f $! a
  | otherwise = speculation `par`
    if cmp guess a
    then speculation
    else f a
  where speculation = f guess
{-# INLINE specBy #-}

-- | 'spec' comparing by projection onto another type
specOn :: Eq c => (a -> c) -> a -> (a -> b) -> a -> b
specOn = specBy . on (==)
{-# INLINE specOn #-}

-- * STM-based speculation

-- | @'specSTM' g f a@ evaluates @fg = do g' <- g; f g'@, while forcing @a@, then if @g' == a@ then @fg@ is returned. Otherwise the side-effects of @fg@ are rolled back and @f a@ is evaluated. @g@ is allowed to be a monadic action, so that we can kickstart the computation of @a@ earlier. Under high load, or when we are not using the parallel runtime, the speculation is avoided, to enable this to more closely approximate the runtime profile of spec.
--
-- If the argument @a@ is already evaluated, we don\'t bother to perform @f g@ at all.
--
-- If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task.
--
-- However, if the guess isn\'t available more cheaply than the actual answer then this saves no work, and if the guess is
-- wrong, you risk evaluating the function twice.
--
-- The best-case timeline looks like:
--
-- > foreground: [--- g >>= f ---]
-- > spark:          [------- a -------]
-- > foreground:                       [-] (compare g' == a)
-- > overall:    [---- specSTM g f a ----]
--
-- The worst-case timeline looks like:
--
-- > foreground: [---- g >>= f ----]
-- > spark:         [------- a -------]
-- > foreground:                      [-] (check if g' == a)
-- > foreground:                        [--] (rollback)
-- > foreground:                           [------ f a ------]
-- > overall:    [------------ specSTM g f a ----------------]
--
-- Under high load, 'specSTM' degrades less gracefully than 'spec':
--
-- > foreground: [---- g >>= f ----]
-- > spark:                        [------- a -------]
-- > foreground:                                     [-] (check if g' == a)
-- > foreground:                                       [--] (rollback)
-- > foreground:                                          [------ f a ------]
-- > overall:    [--------------------specSTM g f a ------------------------]
--
-- Compare these to the timeline of @f $! a@:
--
-- > foreground: [------- a -------]
-- > foreground:                   [------ f a ------]
--

specSTM :: Eq a => STM a -> (a -> STM b) -> a -> STM b
specSTM = specBySTM (returning (==))
{-# INLINE specSTM #-}

-- | 'specSTM' using a user defined comparison function
specBySTM :: (a -> a -> STM Bool) -> STM a -> (a -> STM b) -> a -> STM b
specBySTM cmp mguess f a = do
  sparks <- unsafeIOToSTM numSparks
  if sparks < numCapabilities
    then a `par` do
      guess <- mguess
      result <- f guess
      -- rendezvous with a
      matching <- cmp guess a
      unless matching retry
      return result
     `orElse`
      f a
    else f $! a
{-# INLINE specBySTM #-}

-- | @'specBySTM' . 'on' (==)@
specOnSTM :: Eq c => (a -> STM c) -> STM a -> (a -> STM b) -> a -> STM b
specOnSTM = specBySTM . on (liftM2 (==))
{-# INLINE specOnSTM #-}
