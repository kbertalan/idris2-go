module MoreTypes2

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "structs.go"
              (package "main")
              [ import_ "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field "X" $ int
                   , field "Y" $ int
                  ]
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ compositL (tid' "Vertex") [intL 1, intL 2] ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

