module Basics1

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "packages.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math/rand"
              ]
              [ func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [stringL "My favorite number is", call (id_ "rand" /./ "Intn") [intL 10]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

