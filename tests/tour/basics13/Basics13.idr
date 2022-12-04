module Basics13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [id_ "x", id_ "y"] (tid' "int") [int 3, int 4] ]
                , decl $ vars [ var [id_ "f"] (tid' "float64") [
                    call (id_ "math" /./ "Sqrt") [
                      cast_ (tid' "float64") ( id_ "x" /*/ id_ "x" /+/ id_ "y" /*/ id_ "y" )
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

