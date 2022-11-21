module Basics10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "short-variable-declarations.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars
                  [ var (map id' ["i", "j"]) (id' "int") [int 1, int 2]
                  ]
                , [id' "k"] /:=/ [int 3]
                , decl $ vars
                  [ var' (map id' ["c", "python", "java"]) [bool True, bool False, string "no!"]
                    ]
                , expr $ call (id' "fmt" /./ "Println") [id' "i", id' "j", id' "k", id' "c", id' "python", id' "java"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

