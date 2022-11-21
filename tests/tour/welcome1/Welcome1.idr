module Welcome1

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "hello.go"
              (package "main")
              [import' "fmt"]
              [ func (id' "main") [] void [
                expr $ call (id' "fmt" /./ "Println") [string "Hello,   "]
              ]]


  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

