module Basics16

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "numeric-constants.go"
              (package "main")
              [ import_ "fmt" ]
              [ consts
                [ const' [id_ "Big"] [intL 1 /<</ intL 100]
                  |> docs [ " Create a huge number by shifting a 1 bit left 100 places."
                          , " In other words, the binary number that is 1 followed by 100 zeroes."
                          ]
                , const' [id_ "Small"] [id_ "Big" />>/ intL 99]
                  |> doc " Shift it right again 99 places, so we end up with 1<<1, or 2."
                ]
              , func "needInt" [field "x" int] [fieldT int]
                [ return [ id_ "x" /*/ intL 10 /+/ intL 1 ] ]
              , func "needFloat" [field "x" float64] [fieldT float64]
                [ return [ id_ "x" /*/ floatL 0.1 ] ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [call (id_ "needInt") [id_ "Small"]]
                , expr $ call (id_ "fmt" /./ "Println") [call (id_ "needFloat") [id_ "Small"]]
                , expr $ call (id_ "fmt" /./ "Println") [call (id_ "needFloat") [id_ "Big"]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()
