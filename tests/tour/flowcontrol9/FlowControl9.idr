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
              [ func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Print") [ string "Go runs on " ]
                , switchS ([id_ "os"] /:=/ [id_ "runtime" /./ "GOOS"]) (id_ "os")
                  [ case_ [string "darwin"]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "OS X."] ]
                  , case_ [string "linux"]
                    [ expr $ call (id_ "fmt" /./ "Println") [string "Linux."] ]
                  , default_
                    [ expr (call (id_ "fmt" /./ "Printf") [string "%s os.\\n", id_ "os"])
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

