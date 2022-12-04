module Basics9

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "variables-with-initializers.go"
              (package "main")
              [ import' "fmt" ]
              [ vars
                [ var (map id_ ["i", "j"]) int [intL 1, intL 2]
                ]
              , func (id_ "main") [] void
                [ decl $ vars
                  [ var' (map id_ ["c", "python", "java"]) [boolL True, boolL False, stringL "no!"]
                    ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "i", id_ "j", id_ "c", id_ "python", id_ "java"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

