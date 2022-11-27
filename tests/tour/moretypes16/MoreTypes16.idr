module MoreTypes16

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range.go"
              (package "main")
              [ import' "fmt" ]
              [ vars [ var' [ id' "pow" ] [ composite (array' $ id' "int")
                [ int 1, int 2, int 4, int 8, int 16, int 32, int 64, int 128 ] ] ]
              , func (id' "main") [] void
                [ rangeKV "i" "v" (id' "pow")
                  [ expr $ call (id' "fmt" /./ "Printf") [ string "2**%d = %d\\n", id' "i", id' "v" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

