-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Speculation.Class
-- Copyright   :  (C) 2011 Edward Kmett, Jake McArthur
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Versions of the combinators from the 'speculation' package
-- with the signature rearranged to enable them to be used
-- directly as actions in the 'Cont' and 'ContT' monads
-- or any other 'Codensity'-shaped monad.
----------------------------------------------------------------------------
module Control.Concurrent.Speculation.Class where

import Control.Monad.Trans.Cont
import Control.Concurrent.Speculation

class MonadSpec m where
  -- | When a is unevaluated, @'spec' g a@ evaluates the current continuation 
  -- with @g@ while testing if @g@ '==' @a@, if they differ, it re-evalutes the
  -- continuation with @a@. If @a@ was already evaluated, the continuation is
  -- just directly applied to @a@ instead.
  specM :: Eq a => a -> a -> m a

  -- | As per 'spec', without the check for whether or not the second argument
  -- is already evaluated.
  specM' :: Eq a => a -> a -> m a

  -- | @spec@ with a user supplied comparison function
  specByM :: (a -> a -> Bool) -> a -> a -> m a

  -- | @spec'@ with a user supplied comparison function
  specByM' :: (a -> a -> Bool) -> a -> a -> m a

  -- | @spec'@ with a user supplied comparison function
  specOnM :: Eq c => (a -> c) -> a -> a -> m a

  -- | @spec'@ with a user supplied comparison function
  specOnM' :: Eq c => (a -> c) -> a -> a -> m a

-- * Basic speculation

instance Monad m => MonadSpec (ContT r m) where
  specM g a = ContT $ \k -> spec g k a
  specM' g a = ContT $ \k -> spec' g k a
  specByM f g a = ContT $ \k -> specBy f g k a
  specByM' f g a = ContT $ \k -> specBy' f g k a
  specOnM f g a = ContT $ \k -> specOn f g k a
  specOnM' f g a = ContT $ \k -> specOn' f g k a
