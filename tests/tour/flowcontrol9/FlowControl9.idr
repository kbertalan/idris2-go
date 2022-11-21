module FlowControl9

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "switch.go"
              (package "main")
              [ import' "fmt"
              , import' "runtime"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Print") [ string "Go runs on " ]
                , switchs ([id' "os"] /:=/ [id' "runtime" /./ "GOOS"]) (id' "os")
                  [ case' [string "darwin"]
                    [ expr $ call (id' "fmt" /./ "Println") [string "OS X."] ]
                  , case' [string "linux"]
                    [ expr $ call (id' "fmt" /./ "Println") [string "Linux."] ]
                  , default'
                    [ expr (call (id' "fmt" /./ "Printf") [string "%s os.\\n", id' "os"])
                      |> docs
                        [ " freebsd, openbsd,"
                        , " plan9, windows..."
                        ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

