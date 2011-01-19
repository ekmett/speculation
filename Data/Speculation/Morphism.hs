{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Speculation.Morphism
    ( hylo
    ) where

import GHC.Prim
import GHC.Types

import Data.Speculation

{-
newtype Mu f = In { out :: f (Mu f) } 

ana :: (Functor f, Eq a) => (Int -> a) -> (a -> f a) -> a -> Mu f
ana g psi = go 0# 
  where
    go n = In . fmap (go (n +# 1#)) . spec (g (I# n)) psi

apo :: (Functor f, Eq a) => (Int -> a) -> (a -> f (Either (Mu f) a)) -> a -> Mu f
apo g psi = go 0#
    where
        go n = In . fmap (either id (go (n +# 1#))) . spec (g (I# n)) psi 
-}
        
-- | @'hylo' g phi psi@ is a hylomorphism using a speculative anamorphism, where
-- @g n@ estimates the seed after n iterations of 'psi'.

hylo :: (Functor f, Eq a) => (Int -> a) -> (f b -> b) -> (a -> f a) -> a -> b
hylo g phi psi = go 0#
    where
        go n = phi . fmap (go (n +# 1#)) . spec (g (I# n)) psi
