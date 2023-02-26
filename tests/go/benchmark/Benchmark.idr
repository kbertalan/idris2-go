module Benchmark

fibTR : Int -> Int -> Int -> Int
fibTR n1 _ 0 = n1
fibTR _ n2 1 = n2
fibTR n1 n2 n = fibTR n2 (n1+n2) (n-1)

fib : Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fibTR 1 1 n

main : IO ()
main = do
  printLn $ fib 10
