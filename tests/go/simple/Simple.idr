module Simple

import System

main : IO ()
main = do
  putStrLn "executing a system call"
  code <- System.system "echo something"
  putStrLn $ show code

  putStrLn "printing numbers"
  for_ [1,2,3] $ \i => do
    printLn i

  putStrLn "text in color"
  putStrLn "\x1b[1;31mHello \x1b[1;34mWorld!\x1b[1;0m"

  putStrLn "unpack a string and display it one by one"
  let theString = "this is the unpacked string"
  for_ (unpack theString) $ \c => do
    putChar c
  putChar '\n'

  putStrLn "manipulating env"
  let envVarName = "TEST_ENV_VAR"
  printLn !(setEnv envVarName "a" True)
  printLn !(setEnv envVarName "b" False)
  Just value <- getEnv envVarName
    | Nothing => putStrLn "there was no env variable \{envVarName}"
  printLn $ filter (\(n,v) => n == envVarName) !getEnvironment
  printLn !(unsetEnv envVarName)
  Nothing <- getEnv envVarName
    | Just value => putStrLn "there was an env variable \{envVarName} with value \{value}"

  putStrLn "done"

