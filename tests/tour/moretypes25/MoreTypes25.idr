module MoreTypes25

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "function-closures.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "adder" [] [fieldT $ func' [fieldT int] [fieldT int] ]
                [ [ id_ "sum" ] /:=/ [ intL 0 ]
                , return [ funcL [field "x" int] [ fieldT int ]
                    [ [ id_ "sum" ] /+=/ [ id_ "x" ]
                    , return [ id_ "sum" ]
                    ]
                  ]
                ]
              , func "main" [] void
                [ [ id_ "pos", id_ "neg" ] /:=/ [ call (id_ "adder") [], call (id_ "adder") [] ]
                , for_ ([ id_ "i" ] /:=/ [ intL 0 ]) (id_ "i" /</ intL 10) (inc $ id_ "i")
                  [ expr $ call (id_ "fmt" /./ "Println")
                    [ call (id_ "pos") [ id_ "i" ]
                    , call (id_ "neg") [ intL (-2) /*/ id_ "i" ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

