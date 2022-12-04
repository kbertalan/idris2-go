module FlowControl6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "if-with-a-short-statement.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id_ "pow") [field ["x","n","lim"] float64] [field [] float64]
                [ ifS ([id_ "v"] /:=/ [call (id_ "math" /./ "Pow") [id_ "x", id_ "n"]]) (id_ "v" /</ id_ "lim")
                  [ return [ id_ "v" ] ]
                , return [ id_ "lim" ]
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "pow") [intL 3, intL 2, intL 10]
                  , call (id_ "pow") [intL 3, intL 3, intL 20]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

