module FlowControl13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "defer-multi.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [string "counting"]
                , for_ ([id_ "i"] /:=/ [int 0]) (id_ "i" /</ int 10) (inc $ id_ "i")
                  [ defer $ call (id_ "fmt" /./ "Println") [id_ "i"]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [string "done"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

