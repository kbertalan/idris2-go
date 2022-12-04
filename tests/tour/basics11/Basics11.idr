module Basics11

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt"
              , import' "math/cmplx"
              ]
              [ vars
                [ var [id_ "ToBe"] (tid' "bool") [bool False]
                , var [id_ "MaxInt"] (tid' "uint64") [int 1 /<</ int 64 /-/ int 1]
                , var [id_ "z"] (tid' "complex128") [call (id_ "cmplx" /./ "Sqrt") [int (-5) /+/ imag 12]]
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id_ "ToBe", id_ "ToBe"]
                , expr $ call (id_ "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id_ "MaxInt", id_ "MaxInt"]
                , expr $ call (id_ "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id_ "z", id_ "z"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

