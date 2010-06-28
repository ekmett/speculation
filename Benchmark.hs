module Main where

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = defaultMain 
    [ bench "fib 1" $ nf fib 1
    , bench "fib 2" $ nf fib 2
    ]
