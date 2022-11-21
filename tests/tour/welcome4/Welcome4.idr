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
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "Welcome to the playground!"]
                , expr $ call (id' "fmt" /./ "Println") [string "The time is", call (id' "time.Now") []]
                ]
              ]


  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

