module MoreTypes22

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "map-literals-continued.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "m" ] /:=/ [ call (id_ "make") [map_ (id_ "string") (id_ "int")] ]
                , [ id_ "m" `index` string "Answer" ] /=/ [ int 42 ]
                , expr $ call (id_ "fmt" /./ "Println") [ string "The value:", id_ "m" `index` string "Answer"]
                , [ id_ "m" `index` string "Answer" ] /=/ [ int 48 ]
                , expr $ call (id_ "fmt" /./ "Println") [ string "The value:", id_ "m" `index` string "Answer"]
                , expr $ call (id_ "delete") [ id_ "m", string "Answer" ]
                , expr $ call (id_ "fmt" /./ "Println") [ string "The value:", id_ "m" `index` string "Answer"]
                , [ id_ "v", id_ "ok" ] /:=/ [ id_ "m" `index` string "Answer" ]
                , expr $ call (id_ "fmt" /./ "Println") [ string "The value:", id_ "v", string "Present?", id_ "ok" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

