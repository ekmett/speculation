{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.STM.Speculation 
    ( specSTM
    , specSTM'
    , specOnSTM
    , specOnSTM'
    , specBySTM
    , specBySTM'
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Speculation (evaluated)
import Control.Exception (Exception, throw, fromException)
import Control.Parallel (par)
import Data.Typeable (Typeable)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)

newtype Speculation = Speculation Int deriving (Show,Eq,Typeable)
instance Exception Speculation

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

specSTM :: Eq a => a -> (a -> STM b) -> a -> STM b
specSTM = specBySTM (==) 
{-# INLINE specSTM #-}

-- | Unlike 'specSTM', 'specSTM'' doesn't check if the argument has already been evaluated.

specSTM' :: Eq a => a -> (a -> STM b) -> a -> STM b
specSTM' = specBySTM' (==) 
{-# INLINE specSTM' #-}

-- | 'specSTM' using a user defined comparison function
specBySTM :: (a -> a -> Bool) -> a -> (a -> STM b) -> a -> STM b
specBySTM cmp g f a 
    | evaluated a = f a 
    | otherwise   = specBySTM' cmp g f a
{-# INLINE specBySTM #-}

-- | 'specSTM'' using a user defined comparison function
specBySTM' :: (a -> a -> Bool) -> a -> (a -> STM b) -> a -> STM b
specBySTM' cmp g f a = a `par` do
    exn <- freshSpeculation
    let 
      try = do
        result <- f g 
        if cmp g a
          then return result
          else throw exn
    try `catchSTM` \e -> case fromException e of
        Just exn' | exn == exn' -> f a -- rerun with alternative inputs
        _ -> throw e                   -- this is somebody else's problem

{-# INLINE specBySTM' #-}

-- | 'specOnSTM' . 'on' (==)'
specOnSTM :: Eq c => (a -> c) -> a -> (a -> STM b) -> a -> STM b
specOnSTM = specBySTM . on (==)
{-# INLINE specOnSTM #-}

-- | 'specOnSTM'' . 'on' (==)'
specOnSTM' :: Eq c => (a -> c) -> a -> (a -> STM b) -> a -> STM b
specOnSTM' = specBySTM' . on (==)
{-# INLINE specOnSTM' #-}

speculationSupply :: TVar Int
speculationSupply = unsafePerformIO $ newTVarIO 0
{-# NOINLINE speculationSupply #-}

freshSpeculation :: STM Speculation
freshSpeculation = do
    n <- readTVar speculationSupply
    writeTVar speculationSupply $! n + 1
    return (Speculation n)
{-# INLINE freshSpeculation #-}

