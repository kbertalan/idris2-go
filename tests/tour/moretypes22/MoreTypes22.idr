module MoreTypes22

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "map-literals-continued.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "m" ] /:=/ [ call (id' "make") [map' (id' "string") (id' "int")] ]
                , [ id' "m" `index` string "Answer" ] /=/ [ int 42 ]
                , expr $ call (id' "fmt" /./ "Println") [ string "The value:", id' "m" `index` string "Answer"]
                , [ id' "m" `index` string "Answer" ] /=/ [ int 48 ]
                , expr $ call (id' "fmt" /./ "Println") [ string "The value:", id' "m" `index` string "Answer"]
                , expr $ call (id' "delete") [ id' "m", string "Answer" ]
                , expr $ call (id' "fmt" /./ "Println") [ string "The value:", id' "m" `index` string "Answer"]
                , [ id' "v", id' "ok" ] /:=/ [ id' "m" `index` string "Answer" ]
                , expr $ call (id' "fmt" /./ "Println") [ string "The value:", id' "v", string "Present?", id' "ok" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

