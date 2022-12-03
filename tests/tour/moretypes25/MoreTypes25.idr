module MoreTypes25

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "function-closures.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "adder") [] [field [] $ func' [field [] $ id' "int"] [field [] $ id' "int"] ]
                [ [ id' "sum" ] /:=/ [ int 0 ]
                , return [ funcL [field ["x"] $ id' "int"] [ field [] $ id' "int" ]
                    [ [ id' "sum" ] /+=/ [ id' "x" ]
                    , return [ id' "sum" ]
                    ]
                  ]
                ]
              , func (id' "main") [] void
                [ [ id' "pos", id' "neg" ] /:=/ [ call (id' "adder") [], call (id' "adder") [] ]
                , for' ([ id' "i" ] /:=/ [ int 0 ]) (id' "i" /</ int 10) (inc $ id' "i")
                  [ expr $ call (id' "fmt" /./ "Println")
                    [ call (id' "pos") [ id' "i" ]
                    , call (id' "neg") [ int (-2) /*/ id' "i" ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

