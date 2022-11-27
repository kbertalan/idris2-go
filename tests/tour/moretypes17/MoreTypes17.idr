module MoreTypes17

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range-continued.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "pow" ] /:=/ [ call (id' "make") [ array' $ id' "int", int 10 ] ]
                , rangeV "i" (id' "pow")
                  [ [ id' "pow" `index` id' "i" ] /=/ [ int 1 /<</ call (id' "uint") [ id' "i" ] ]
                    |> comment " == 2**i"
                  ]
                , rangeKV "_" "value" (id' "pow")
                  [ expr $ call (id' "fmt" /./ "Printf") [ string "%d\\n", id' "value" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

