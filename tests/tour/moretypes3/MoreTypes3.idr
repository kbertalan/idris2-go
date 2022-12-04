module MoreTypes3

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-fields.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field ["X"] $ tid' "int"
                  , field ["Y"] $ tid' "int"
                  ]
                ]
              , func (id_ "main") [] void
                [ [ id_ "v" ] /:=/ [ composit (tid' "Vertex") [int 1, int 2] ]
                , [ id_ "v" /./ "X" ] /=/ [ int 4 ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "v" /./ "X" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

