module FlowControl5

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "if.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "sqrt") [field ["x"] float64] [field [] string]
                [ if_ (id_ "x" /</ intL 0)
                  [ return [ call (id_ "sqrt") [ minus' $ id_ "x" ] /+/ stringL "i" ] ]
                , return [ call (id_ "fmt" /./ "Sprint")
                    [ call (id_ "math" /./ "Sqrt") [id_ "x"]
                    ]
                  ]
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "sqrt") [intL 2]
                  , call (id_ "sqrt") [intL $ -4]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

