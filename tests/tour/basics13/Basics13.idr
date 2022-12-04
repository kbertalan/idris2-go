module Basics13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math"
              ]
              [ func "main" [] void
                [ decl $ vars [ var [id_ "x", id_ "y"] int [intL 3, intL 4] ]
                , decl $ vars [ var [id_ "f"] float64 [
                    call (id_ "math" /./ "Sqrt") [
                      cast_ float64 ( id_ "x" /*/ id_ "x" /+/ id_ "y" /*/ id_ "y" )
                    ]
                  ] ]
                , decl $ vars [ var [id_ "z"] (tid' "uint") [
                    cast_ (tid' "uint") (id_ "f")
                  ] ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "x", id_ "y", id_ "z"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

