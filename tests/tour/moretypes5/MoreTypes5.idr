module MoreTypes5

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

%hide Prelude.Ops.infixl.(|>)
%hide Prelude.(|>)

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import_ "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ fields ["X", "Y"] int
                  ]
                ]
              , vars
                [ var' [id_ "v1"] [ compositL (tid' "Vertex") [intL 1, intL 2] ]
                  |> comment " has type Vertex"
                , var' [id_ "v2"] [ compositL (tid' "Vertex") [id_ "X" /:/ intL 1] ]
                  |> comment " Y:0 is implicit"
                , var' [id_ "v3"] [ compositL (tid' "Vertex") [] ]
                  |> comment " X:0 and Y:0"
                , var' [id_ "p"] [ ptrOf $ compositL (tid' "Vertex") [intL 1, intL 2] ]
                  |> comment " has type *Vertex"
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ id_ "v1", id_ "p", id_ "v2", id_ "v3" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()
