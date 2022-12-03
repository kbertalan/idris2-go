module Basics16

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "numeric-constants.go"
              (package "main")
              [ import' "fmt" ]
              [ consts
                [ const' [id_ "Big"] (Maybe Identifier `the` Nothing) [int 1 /<</ int 100]
                  |> docs [ " Create a huge number by shifting a 1 bit left 100 places."
                          , " In other words, the binary number that is 1 followed by 100 zeroes."
                          ]
                , const' [id_ "Small"] (Maybe Identifier `the` Nothing) [id_ "Big" />>/ int 99]
                  |> doc " Shift it right again 99 places, so we end up with 1<<1, or 2."
                ]
              , func (id_ "needInt") [field ["x"] (id_ "int")] [field [] $ id_ "int"]
                [ return [ id_ "x" /*/ int 10 /+/ int 1 ] ]
              , func (id_ "needFloat") [field ["x"] (id_ "float64")] [field [] $ id_ "float64"]
                [ return [ id_ "x" /*/ float 0.1 ] ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [call (id_ "needInt") [id_ "Small"]]
                , expr $ call (id_ "fmt" /./ "Println") [call (id_ "needFloat") [id_ "Small"]]
                , expr $ call (id_ "fmt" /./ "Println") [call (id_ "needFloat") [id_ "Big"]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

