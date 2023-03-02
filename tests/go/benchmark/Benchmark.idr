module Benchmark

import Data.SnocList

fibTR : Int -> Int -> Int -> Int
fibTR n1 _ 0 = n1
fibTR _ n2 1 = n2
fibTR n1 n2 n = fibTR n2 (n1+n2) (n-1)

fib : Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fibTR 1 1 n

iterateTR : Int -> (a -> a) -> a -> List a
iterateTR n f = go n Lin
  where go : Int -> SnocList a -> a -> List a
        go 0 sx _ = sx <>> Nil
        go n sx v = go (n-1) (sx :< v) (f v)

bind : Int -> Nat
bind til =
  length $ [1..til] >>= (\n => iterateTR n (+1) n)

main : IO ()
main = do
  printLn $ fib 10
  printLn $ bind 10

