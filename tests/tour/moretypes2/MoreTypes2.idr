module MoreTypes2

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "structs.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ Go.type "Vertex" [] $ struct
                  [ field ["X"] $ id' "int"
                  , field ["Y"] $ id' "int"
                  ]
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [ composite (id' "Vertex") [int 1, int 2] ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

