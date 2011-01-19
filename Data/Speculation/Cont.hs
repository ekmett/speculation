-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Speculation.Cont
-- Copyright   :  (C) 2011 Edward Kmett, Jake McArthur
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Versions of the combinators from the 'speculation' package
-- with the signature rearranged to enable them to be used
-- directly as actions in the 'Cont' and 'ContT' monads.
----------------------------------------------------------------------------
module Data.Speculation.Cont where

import Control.Monad.Trans.Cont
import qualified Data.Speculation as Prim
import Control.Concurrent.STM

-- * Basic speculation

-- | When a is unevaluated, @'spec' g a@ evaluates the current continuation 
-- with @g@ while testing if @g@ '==' @a@, if they differ, it re-evalutes the
-- continuation with @a@. If @a@ was already evaluated, the continuation is
-- just directly applied to @a@ instead.
spec :: Eq a => a -> a -> ContT r m a
spec g a = ContT $ \k -> Prim.spec g k a 

-- | As per 'spec', without the check for whether or not the second argument
-- is already evaluated.
spec' :: Eq a => a -> a -> ContT r m a
spec' g a = ContT $ \k -> Prim.spec' g k a

-- | @spec@ with a user supplied comparison function
specBy :: (a -> a -> Bool) -> a -> a -> ContT r m a
specBy f g a = ContT $ \k -> Prim.specBy f g k a

-- | @spec'@ with a user supplied comparison function
specBy' :: (a -> a -> Bool) -> a -> a -> ContT r m a
specBy' f g a = ContT $ \k -> Prim.specBy' f g k a

-- | @spec'@ with a user supplied comparison function
specOn :: Eq c => (a -> c) -> a -> a -> ContT r m a
specOn f g a = ContT $ \k -> Prim.specOn f g k a

-- | @spec'@ with a user supplied comparison function
specOn' :: Eq c => (a -> c) -> a -> a -> ContT r m a
specOn' f g a = ContT $ \k -> Prim.specOn' f g k a

-- * STM-based speculation

specSTM :: Eq a => STM a -> a -> ContT r STM a
specSTM g a = ContT $ \k -> Prim.specSTM g k a 

specSTM' :: Eq a => STM a -> a -> ContT r STM a
specSTM' g a = ContT $ \k -> Prim.specSTM' g k a 

specOnSTM :: Eq c => (a -> STM c) -> STM a -> a -> ContT r STM a
specOnSTM f g a = ContT $ \k -> Prim.specOnSTM f g k a 

specOnSTM' :: Eq c => (a -> STM c) -> STM a -> a -> ContT r STM a
specOnSTM' f g a = ContT $ \k -> Prim.specOnSTM' f g k a 

specBySTM :: (a -> a -> STM Bool) -> STM a -> a -> ContT r STM a
specBySTM f g a = ContT $ \k -> Prim.specBySTM f g k a 

specBySTM' :: (a -> a -> STM Bool) -> STM a -> a -> ContT r STM a
specBySTM' f g a = ContT $ \k -> Prim.specBySTM' f g k a 

