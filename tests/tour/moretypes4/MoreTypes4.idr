module MoreTypes4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-pointers.go"
              (package "main")
              [ import_ "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field "X" int
                  , field "Y" int
                  ]
                ]
              , func "main" [] void
                [ [ id_ "v" ] /:=/ [ compositL (tid' "Vertex") [intL 1, intL 2] ]
                , [ id_ "p" ] /:=/ [ /&/ id_ "v" ]
                , [ id_ "p" /./ "X" ] /=/ [ 1 `expL` 9 ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "v" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

