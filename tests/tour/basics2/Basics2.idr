module Basics2

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "imports.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math"
              ]
              [ func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Printf") [stringL "Now you have %g problems.\n", call (id_ "math" /./ "Sqrt") [intL 7]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

