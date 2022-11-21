module MoreTypes5

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ Go.type "Vertex" [] $ struct
                  [ field ["X", "Y"] $ id' "int"
                  ]
                ]
              , vars
                [ var' [id' "v1"] [ composite (id' "Vertex") [int 1, int 2] ]
                  |> comment " has type Vertex"
                , var' [id' "v2"] [ composite (id' "Vertex") [id' "X" /:/ int 1] ]
                  |> comment " Y:0 is implicit"
                , var' [id' "v3"] [ composite (id' "Vertex") [] ]
                  |> comment " X:0 and Y:0"
                , var' [id' "p"] [ /&/ composite (id' "Vertex") [int 1, int 2] ]
                  |> comment " has type *Vertex"
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [ id' "v1", id' "p", id' "v2", id' "v3" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

