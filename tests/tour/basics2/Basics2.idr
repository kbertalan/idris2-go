module Basics2

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "imports.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Printf") [string "Now you have %g problems.\\n", call (id_ "math" /./ "Sqrt") [int 7]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

