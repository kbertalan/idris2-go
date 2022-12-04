module FlowControl10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "switch-evaluation-order.go"
              (package "main")
              [ import_ "fmt"
              , import_ "time"
              ]
              [ func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ stringL "When's Sunday?" ]
                , [ id_ "today" ] /:=/ [ call (call (id_ "time" /./ "Now") [] /./ "Weekday") [] ]
                , switch (id_ "time" /./ "Saturday")
                  [ case_ [id_ "today" /+/ intL 0]
                    [ expr $ call (id_ "fmt" /./ "Println") [stringL "Today."] ]
                  , case_ [id_ "today" /+/ intL 1]
                    [ expr $ call (id_ "fmt" /./ "Println") [stringL "Tomorrow."] ]
                  , case_ [id_ "today" /+/ intL 2]
                    [ expr $ call (id_ "fmt" /./ "Println") [stringL "In two days."] ]
                  , default_
                    [ expr (call (id_ "fmt" /./ "Println") [stringL "Too far away."]) ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

