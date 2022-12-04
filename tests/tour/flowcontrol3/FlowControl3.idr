module FlowControl3

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "for-is-gos-while.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [id_ "sum"] /:=/ [intL 1]
                , while (id_ "sum" /</ intL 1000)
                  [ [id_ "sum" ] /+=/ [id_ "sum"] ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "sum"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

