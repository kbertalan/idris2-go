module Simple

import Data.String
import System
import System.File

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

  putStrLn "writing file"
  let fileName = "./file-under-test.txt"
      fileContent = unlines $ [ "line \{show i}" | i <- [the Int 1..1000] ]
  Right () <- writeFile fileName fileContent
    | Left e => putStrLn $ show e
  putStrLn "reading file"
  Right readContent <- readFile fileName
    | Left e => putStrLn $ show e

  if fileContent == readContent then putStrLn "contents are matching"
                                else putStrLn "contents are different\nexpected:\n\{fileContent}\n\ngot:\n\{readContent}"

  Right () <- removeFile fileName
    | Left e => putStrLn $ show e

  pure ()

