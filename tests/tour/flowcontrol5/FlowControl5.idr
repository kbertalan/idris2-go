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
              [ func (id' "sqrt") [field ["x"] $ id' "float64"] [field [] $ id' "string"]
                [ if' (id' "x" /</ int 0)
                  [ return [ call (id' "sqrt") [ minus' $ id' "x" ] /+/ string "i" ] ]
                , return [ call (id' "fmt" /./ "Sprint")
                    [ call (id' "math" /./ "Sqrt") [id' "x"]
                    ]
                  ]
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println")
                  [ call (id' "sqrt") [int 2]
                  , call (id' "sqrt") [int $ -4]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

