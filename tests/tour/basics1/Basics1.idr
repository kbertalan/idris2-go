module Basics1

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "packages.go"
              (package "main")
              [ import' "fmt"
              , import' "math/rand"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "My favorite number is", call (id' "rand" /./ "Intn") [int 10]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

