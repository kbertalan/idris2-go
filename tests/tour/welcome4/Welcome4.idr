module Welcome4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "sandbox.go"
              (package "main")
              [ import' "fmt"
              , import' "time"
              ]
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [stringL "Welcome to the playground!"]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "The time is", call (id_ "time.Now") []]
                ]
              ]


  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

