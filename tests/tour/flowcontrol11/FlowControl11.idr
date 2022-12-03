module FlowControl11

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "switch-with-no-condition.go"
              (package "main")
              [ import' "fmt"
              , import' "time"
              ]
              [ func (id_ "main") [] void
                [ [ id_ "t" ] /:=/ [ call (id_ "time" /./ "Now") [] ]
                , switch'
                  [ case_ [call (id_ "t" /./ "Hour") [] /</ int 12]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "Good morning!"] ]
                  , case_ [call (id_ "t" /./ "Hour") [] /</ int 17]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "Good afternoon."] ]
                  , default_
                    [ expr (call (id_ "fmt" /./ "Println") [string "Good evening."]) ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

