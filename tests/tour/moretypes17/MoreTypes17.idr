module MoreTypes17

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range-continued.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "pow" ] /:=/ [ call (id_ "make") [ array' $ id_ "int", int 10 ] ]
                , rangeV "i" (id_ "pow")
                  [ [ id_ "pow" `index` id_ "i" ] /=/ [ int 1 /<</ call (id_ "uint") [ id_ "i" ] ]
                    |> comment " == 2**i"
                  ]
                , rangeKV "_" "value" (id_ "pow")
                  [ expr $ call (id_ "fmt" /./ "Printf") [ string "%d\\n", id_ "value" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

