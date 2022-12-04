module MoreTypes5

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field ["X", "Y"] $ tid' "int"
                  ]
                ]
              , vars
                [ var' [id_ "v1"] [ composit (tid' "Vertex") [int 1, int 2] ]
                  |> comment " has type Vertex"
                , var' [id_ "v2"] [ composit (tid' "Vertex") [id_ "X" /:/ int 1] ]
                  |> comment " Y:0 is implicit"
                , var' [id_ "v3"] [ composit (tid' "Vertex") [] ]
                  |> comment " X:0 and Y:0"
                , var' [id_ "p"] [ /&/ composit (tid' "Vertex") [int 1, int 2] ]
                  |> comment " has type *Vertex"
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ id_ "v1", id_ "p", id_ "v2", id_ "v3" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

