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
                  [ field ["X"] int
                  , field ["Y"] int
                  ]
                ]
              , func (id_ "main") [] void
                [ [ id_ "v" ] /:=/ [ compositL (tid' "Vertex") [intL 1, intL 2] ]
                , [ id_ "v" /./ "X" ] /=/ [ intL 4 ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "v" /./ "X" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

