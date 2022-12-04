module FlowControl13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "defer-multi.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [stringL "counting"]
                , for_ ([id_ "i"] /:=/ [intL 0]) (id_ "i" /</ intL 10) (inc $ id_ "i")
                  [ defer $ call (id_ "fmt" /./ "Println") [id_ "i"]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "done"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

