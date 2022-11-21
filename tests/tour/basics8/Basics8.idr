module Basics8

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "variables.go"
              (package "main")
              [ import' "fmt" ]
              [ vars
                [ var (map id' ["c", "python", "java"]) (id' "bool") []
                ]
              , func (id' "main") [] void
                [ decl $ vars [ var [id' "i"] (id' "int") [] ]
                , expr $ call (id' "fmt" /./ "Println") [id' "i", id' "c", id' "python", id' "java"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

