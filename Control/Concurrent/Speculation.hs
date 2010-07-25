{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable, MagicHash #-}
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
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Speculation.Internal (returning)
import Data.TagBits (unsafeIsEvaluated)
import Control.Parallel (par)
import Control.Monad (liftM2, unless)
import Data.Function (on)

-- * Basic speculation

-- | @'spec' g f a@ evaluates @f g@ while forcing @a@, if @g == a@ then @f g@ is returned, otherwise @f a@ is evaluated and returned. Furthermore, if the argument has already been evaluated, we skip the @f g@ computation entirely. If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task. However, if the guess isn\'t available more cheaply than the actual answer, then this saves no work and if the guess is wrong, you risk evaluating the function twice. Under high load, since 'f g' is computed via the spark queue, the speculation will be skipped and you will obtain the same answer as 'f $! a'.
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

-- | @'specSTM' g f a@ evaluates @fg = do g' <- g; f g'@, while forcing @a@, then if @g' == a@ then @fg@ is returned. Otherwise the side-effects of @fg@ are rolled back and @f a@ is evaluated. @g@ is allowed to be a monadic action, so that we can kickstart the computation of @a@ earlier.
--
-- If the argument @a@ is already evaluated, we don\'t bother to perform @fg@ at all.
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
