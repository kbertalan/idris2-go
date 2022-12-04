module FlowControl1

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "for.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [id_ "sum"] /:=/ [intL 0]
                , for_ ([id_ "i"] /:=/ [intL 0]) (id_ "i" /</ intL 10) (inc $ id_ "i")
                  [ [id_ "sum" ] /+=/ [id_ "i"] ]
                , expr $ call (id_ "fmt" /./ "Println") [id_ "sum"]
                ]
              ]


  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

