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
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ string "When's Sunday?" ]
                , [ id_ "today" ] /:=/ [ call (call (id_ "time" /./ "Now") [] /./ "Weekday") [] ]
                , switch (id_ "time" /./ "Saturday")
                  [ case_ [id_ "today" /+/ int 0]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "Today."] ]
                  , case_ [id_ "today" /+/ int 1]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "Tomorrow."] ]
                  , case_ [id_ "today" /+/ int 2]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "In two days."] ]
                  , default_
                    [ expr (call (id_ "fmt" /./ "Println") [string "Too far away."]) ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

