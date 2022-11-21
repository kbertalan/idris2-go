module FlowControl13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "defer-multi.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "counting"]
                , for' ([id' "i"] /:=/ [int 0]) (id' "i" /</ int 10) (inc $ id' "i")
                  [ defer $ call (id' "fmt" /./ "Println") [id' "i"]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [string "done"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

