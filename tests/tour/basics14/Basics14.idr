module Basics14

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [id_ "v"] /:=/ [intL 42] |> comment " change me!"
                , expr $ call (id_ "fmt" /./ "Printf") [stringL "v is of type %T\n", id_ "v"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()
