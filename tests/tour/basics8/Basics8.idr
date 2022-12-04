module Basics8

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "variables.go"
              (package "main")
              [ import' "fmt" ]
              [ vars
                [ var (map id_ ["c", "python", "java"]) (tid' "bool") []
                ]
              , func (id_ "main") [] void
                [ decl $ vars [ var [id_ "i"] (tid' "int") [] ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "i", id_ "c", id_ "python", id_ "java"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

