module FlowControl10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "switch-evaluation-order.go"
              (package "main")
              [ import' "fmt"
              , import' "time"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [ string "When's Sunday?" ]
                , [ id' "today" ] /:=/ [ call (call (id' "time" /./ "Now") [] /./ "Weekday") [] ]
                , switch (id' "time" /./ "Saturday")
                  [ case' [id' "today" /+/ int 0]
                    [ expr $ call (id' "fmt" /./ "Println") [string "Today."] ]
                  , case' [id' "today" /+/ int 1]
                    [ expr $ call (id' "fmt" /./ "Println") [string "Tomorrow."] ]
                  , case' [id' "today" /+/ int 2]
                    [ expr $ call (id' "fmt" /./ "Println") [string "In two days."] ]
                  , default'
                    [ expr (call (id' "fmt" /./ "Println") [string "Too far away."]) ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

