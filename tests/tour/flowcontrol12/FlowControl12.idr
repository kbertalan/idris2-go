module FlowControl12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "defer.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ defer $ call (id_ "fmt" /./ "Println") [stringL "world"]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "hello"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

