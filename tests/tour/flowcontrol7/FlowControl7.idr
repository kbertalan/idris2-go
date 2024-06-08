module FlowControl7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

%hide Prelude.Ops.infixl.(|>)
%hide Prelude.(|>)

main : IO ()
main = do
  let src = file "if-and-else.go"
              (package "main")
              [ import_ "fmt"
              , import_ "math"
              ]
              [ func "pow" [fields ["x","n","lim"] float64] [fieldT float64]
                [ ifSE ([id_ "v"] /:=/ [call (id_ "math" /./ "Pow") [id_ "x", id_ "n"]]) (id_ "v" /</ id_ "lim")
                  [ return [ id_ "v" ] ]
                  $ block [ expr $ call (id_ "fmt" /./ "Printf") [stringL "%g >= %g \n", id_ "v", id_ "lim"] ]
                , return [ id_ "lim" ]
                  |> doc " can't use v here, though"
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "pow") [intL 3, intL 2, intL 10]
                  , call (id_ "pow") [intL 3, intL 3, intL 20]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()
