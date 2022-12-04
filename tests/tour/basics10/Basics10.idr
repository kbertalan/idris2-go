module Basics10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "short-variable-declarations.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars
                  [ var (map id_ ["i", "j"]) (tid' "int") [int 1, int 2]
                  ]
                , [id_ "k"] /:=/ [int 3]
                , decl $ vars
                  [ var' (map id_ ["c", "python", "java"]) [bool True, bool False, string "no!"]
                    ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "i", id_ "j", id_ "k", id_ "c", id_ "python", id_ "java"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

