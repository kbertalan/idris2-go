module MoreTypes2

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "structs.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field ["X"] $ tid' "int"
                   , field ["Y"] $ tid' "int"
                  ]
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ composit (tid' "Vertex") [int 1, int 2] ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

