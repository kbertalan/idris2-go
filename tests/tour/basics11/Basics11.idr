module Basics11

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math/cmplx"
              ]
              [ vars
                [ var [id_ "ToBe"] bool [boolL False]
                , var [id_ "MaxInt"] uint64 [intL 1 /<</ intL 64 /-/ intL 1]
                , var [id_ "z"] complex128 [call (id_ "cmplx" /./ "Sqrt") [intL (-5) /+/ imagL 12]]
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Printf") [stringL "Type: %T Value: %v\\n", id_ "ToBe", id_ "ToBe"]
                , expr $ call (id_ "fmt" /./ "Printf") [stringL "Type: %T Value: %v\\n", id_ "MaxInt", id_ "MaxInt"]
                , expr $ call (id_ "fmt" /./ "Printf") [stringL "Type: %T Value: %v\\n", id_ "z", id_ "z"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

