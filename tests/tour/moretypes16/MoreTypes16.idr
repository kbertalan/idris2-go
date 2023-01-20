module MoreTypes16

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "range.go"
              (package "main")
              [ import_ "fmt" ]
              [ vars [ var' [ id_ "pow" ] [ compositL (array' int)
                [ intL 1, intL 2, intL 4, intL 8, intL 16, intL 32, intL 64, intL 128 ] ] ]
              , func "main" [] void
                [ rangeKV "i" "v" (id_ "pow")
                  [ expr $ call (id_ "fmt" /./ "Printf") [ stringL "2**%d = %d\n", id_ "i", id_ "v" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

