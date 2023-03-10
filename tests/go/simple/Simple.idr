module Simple

import Data.Buffer
import Data.String
import System
import System.Directory
import System.File

main : IO ()
main = do
  putStrLn "executing a system call"
  code <- System.system "echo something"
  putStrLn $ show code

  putStrLn "printing numbers"
  for_ [1,2,3] $ \i => do
    printLn i

  for_ [1.0,2.1,3.2] $ \d => do
    printLn d

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

  putStrLn "listing directory"
  Just cwd <- currentDir
    | Nothing => putStrLn "there is no current directory"
  Right files <- listDir cwd
    | Left err => putStrLn $ show err
  for_ (filter ((/=) "output") files) $ \f => do
    putStrLn f
  putStrLn "listing directory ended"

  putStrLn "testing file reading and buffers"
  Right buffer <- createBufferFromFile "test.ipkg"
    | Left err => putStrLn $ show err

  len <- rawSize buffer
  putStrLn $ show len
  putStr $ !(getString buffer 0 len)
  Just buffer <- newBuffer 8
    | Nothing => putStrLn "could not create a buffer with length 8"
  for_ [0..7] $ \offset => setBits8 buffer offset $ cast offset
  for_ [0..7] $ \offset => putStrLn $ show $ !(getBits8 buffer offset)
  for_ [0..3] $ \offset => putStrLn $ show $ !(getBits16 buffer $ offset*2)
  for_ [0..1] $ \offset => putStrLn $ show $ !(getBits32 buffer $ offset*4)
  putStrLn $ show $ !(getBits64 buffer 0)
  setString buffer 0 "12345678"
  putStrLn !(getString buffer 0 8)
  putStrLn "testing file reading and buffers ended"

  putStrLn $ substr 1 3 "1234567890" 
  pure ()

