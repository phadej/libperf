module Main (main) where

import Test.Tasty.PerfBench

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ bgroup "fibonacci numbers"
    [ bench "fifth"     $ whnf fibo  5
    , bench "tenth"     $ whnf fibo 10
    , bench "twentieth" $ whnf fibo 20
    , bench "id ()"     $ whnf id ()
    ]
  ]
