module Basics3

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "exported-names.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [id_ "math" /./ "Pi"] ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

