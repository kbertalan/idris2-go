module MoreTypes22

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "map-literals-continued.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [ id_ "m" ] /:=/ [ make (map_ string int) [] ]
                , [ id_ "m" `index` stringL "Answer" ] /=/ [ intL 42 ]
                , expr $ call (id_ "fmt" /./ "Println") [ stringL "The value:", id_ "m" `index` stringL "Answer"]
                , [ id_ "m" `index` stringL "Answer" ] /=/ [ intL 48 ]
                , expr $ call (id_ "fmt" /./ "Println") [ stringL "The value:", id_ "m" `index` stringL "Answer"]
                , expr $ call (id_ "delete") [ id_ "m", stringL "Answer" ]
                , expr $ call (id_ "fmt" /./ "Println") [ stringL "The value:", id_ "m" `index` stringL "Answer"]
                , [ id_ "v", id_ "ok" ] /:=/ [ id_ "m" `index` stringL "Answer" ]
                , expr $ call (id_ "fmt" /./ "Println") [ stringL "The value:", id_ "v", stringL "Present?", id_ "ok" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

