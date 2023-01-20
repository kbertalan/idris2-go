module MoreTypes17

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range-continued.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [ id_ "pow" ] /:=/ [ make (array' int) [ intL 10 ] ]
                , rangeV "i" (id_ "pow")
                  [ [ id_ "pow" `index` id_ "i" ] /=/ [ intL 1 /<</ cast_ (tid' "uint") ( id_ "i" ) ]
                    |> comment " == 2**i"
                  ]
                , rangeKV "_" "value" (id_ "pow")
                  [ expr $ call (id_ "fmt" /./ "Printf") [ stringL "%d\n", id_ "value" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

