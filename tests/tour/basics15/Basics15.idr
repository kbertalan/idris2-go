module Basics15

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "constants.go"
              (package "main")
              [ import' "fmt" ]
              [ consts [ const' [id_ "Pi"] [floatL 3.14] ]
              , func (id_ "main") [] void
                [ decl $ consts [ const' [id_ "World"] [stringL "世界"] ]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "Hello", id_ "World"]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "Happy", id_ "Pi", stringL "Day"]
                , decl $ consts [ const' [id_ "Truth"] [boolL True] ]
                , expr $ call (id_ "fmt" /./ "Println") [stringL "Go rules?", id_ "Truth"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

