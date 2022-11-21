module FlowControl3

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "for-is-gos-while.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [id' "sum"] /:=/ [int 1]
                , while (id' "sum" /</ int 1000)
                  [ [id' "sum" ] /+=/ [id' "sum"] ]
                , expr $ call (id' "fmt" /./ "Println") [id' "sum"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

