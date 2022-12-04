module FlowControl5

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "if.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math"
              ]
              [ func "sqrt" [field "x" float64] [fieldT string]
                [ if_ (id_ "x" /</ intL 0)
                  [ return [ call (id_ "sqrt") [ minus' $ id_ "x" ] /+/ stringL "i" ] ]
                , return [ call (id_ "fmt" /./ "Sprint")
                    [ call (id_ "math" /./ "Sqrt") [id_ "x"]
                    ]
                  ]
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "sqrt") [intL 2]
                  , call (id_ "sqrt") [intL $ -4]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

