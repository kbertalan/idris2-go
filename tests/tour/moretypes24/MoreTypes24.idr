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
              [ func (id' "compute")
                [ field ["fn"] $ func'
                  [ field [] $ id' "float64"
                  , field [] $ id' "float64"
                  ] [field [] $ id' "float64"]
                ] [field [] $ id' "float64"]
                [ return [ call (id' "fn") [ int 3, int 4 ] ]
                ]
              , func (id' "main") [] void
                [ [ id' "hypot" ] /:=/ [ funcL [ field ["x", "y"] $ id' "float64" ] [field [] $ id' "float64"]
                    [ return [ call (id' "math" /./ "Sqrt") [ id' "x" /*/ id' "x" /+/ id' "y" /*/ id' "y" ] ]
                    ]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [ call (id' "hypot") [ int 5, int 12 ] ]
                , expr $ call (id' "fmt" /./ "Println") [ call (id' "compute") [ id' "hypot" ] ]
                , expr $ call (id' "fmt" /./ "Println") [ call (id' "compute") [ id' "math" /./ "Pow" ] ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

