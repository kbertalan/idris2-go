module MoreTypes16

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range.go"
              (package "main")
              [ import' "fmt" ]
              [ vars [ var' [ id_ "pow" ] [ composit (array' $ tid' "int")
                [ int 1, int 2, int 4, int 8, int 16, int 32, int 64, int 128 ] ] ]
              , func (id_ "main") [] void
                [ rangeKV "i" "v" (id_ "pow")
                  [ expr $ call (id_ "fmt" /./ "Printf") [ string "2**%d = %d\\n", id_ "i", id_ "v" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

