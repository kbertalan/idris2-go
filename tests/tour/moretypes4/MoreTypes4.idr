module MoreTypes4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-pointers.go"
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
                , [ id_ "p" ] /:=/ [ /&/ id_ "v" ]
                , [ id_ "p" /./ "X" ] /=/ [ 1 `exp` 9 ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "v" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

