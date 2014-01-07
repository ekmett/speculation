{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding ((||),(&&)) 
import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((==>))
-- import Test.HUnit hiding (Test)
import Control.Concurrent.Speculation

main :: IO () 
main = defaultMain tests

ignore :: Functor f => f a -> f () 
ignore = fmap (const ())

tests :: [Test]
tests = 
    [ testGroup "cases" $ zipWith (testCase . show) [1 :: Int ..] $
        [] 
    , testGroup "properties" $ zipWith (testProperty . show) [1 :: Int ..] $ 
        [ property $ \ a -> spec a (*2) a == ((*2) a :: Int)  -- unevaluated
        , property $ \ !a -> spec a (*2) a == ((*2) $! a :: Int) -- evaluated
        ] 
    ]
