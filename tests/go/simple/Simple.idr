module Simple

import System

main : IO ()
main = do
  putStrLn $ "executing system call a"
  code <- System.system "echo something"
  putStrLn $ show code

