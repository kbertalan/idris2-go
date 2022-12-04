module MoreTypes24

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "function-values.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "compute")
                [ field ["fn"] $ func'
                  [ field [] float64
                  , field [] float64
                  ] [field [] float64]
                ] [field [] float64]
                [ return [ call (id_ "fn") [ intL 3, intL 4 ] ]
                ]
              , func (id_ "main") [] void
                [ [ id_ "hypot" ] /:=/ [ funcL [ field ["x", "y"] float64 ] [field [] float64]
                    [ return [ call (id_ "math" /./ "Sqrt") [ id_ "x" /*/ id_ "x" /+/ id_ "y" /*/ id_ "y" ] ]
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ call (id_ "hypot") [ intL 5, intL 12 ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ call (id_ "compute") [ id_ "hypot" ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ call (id_ "compute") [ id_ "math" /./ "Pow" ] ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

