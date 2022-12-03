module MoreTypes25

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "function-closures.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "adder") [] [field [] $ func' [field [] $ id_ "int"] [field [] $ id_ "int"] ]
                [ [ id_ "sum" ] /:=/ [ int 0 ]
                , return [ funcL [field ["x"] $ id_ "int"] [ field [] $ id_ "int" ]
                    [ [ id_ "sum" ] /+=/ [ id_ "x" ]
                    , return [ id_ "sum" ]
                    ]
                  ]
                ]
              , func (id_ "main") [] void
                [ [ id_ "pos", id_ "neg" ] /:=/ [ call (id_ "adder") [], call (id_ "adder") [] ]
                , for_ ([ id_ "i" ] /:=/ [ int 0 ]) (id_ "i" /</ int 10) (inc $ id_ "i")
                  [ expr $ call (id_ "fmt" /./ "Println")
                    [ call (id_ "pos") [ id_ "i" ]
                    , call (id_ "neg") [ int (-2) /*/ id_ "i" ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

