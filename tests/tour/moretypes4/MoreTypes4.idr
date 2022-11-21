module MoreTypes4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ Go.type "Vertex" [] $ struct
                  [ field ["X"] $ id' "int"
                  , field ["Y"] $ id' "int"
                  ]
                ]
              , func (id' "main") [] void
                [ [ id' "v" ] /:=/ [ composite (id' "Vertex") [int 1, int 2] ]
                , [ id' "p" ] /:=/ [ /&/ id' "v" ]
                , [ id' "p" /./ "X" ] /=/ [ 1 `exp` 9 ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "v" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

