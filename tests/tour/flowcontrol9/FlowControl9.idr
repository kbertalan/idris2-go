module FlowControl9

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "switch.go"
              (package "main")
              [ import_ "fmt"
              , import_ "runtime"
              ]
              [ func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Print") [ stringL "Go runs on " ]
                , switchS ([id_ "os"] /:=/ [id_ "runtime" /./ "GOOS"]) (id_ "os")
                  [ case_ [stringL "darwin"]
                    [ expr $ call (id_ "fmt" /./ "Println") [stringL "OS X."] ]
                  , case_ [stringL "linux"]
                    [ expr $ call (id_ "fmt" /./ "Println") [stringL "Linux."] ]
                  , default_
                    [ expr (call (id_ "fmt" /./ "Printf") [stringL "%s os.\n", id_ "os"])
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

