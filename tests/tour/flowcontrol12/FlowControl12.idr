module FlowControl12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "defer.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ defer $ call (id' "fmt" /./ "Println") [string "world"]
                , expr $ call (id' "fmt" /./ "Println") [string "hello"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

