module Basics15

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "constants.go"
              (package "main")
              [ import' "fmt" ]
              [ consts [ const' [id_ "Pi"] (Maybe Identifier `the` Nothing) [float 3.14] ]
              , func (id_ "main") [] void
                [ decl $ consts [ const' [id_ "World"] (Maybe Identifier `the` Nothing) [string "世界"] ]
                , expr $ call (id_ "fmt" /./ "Println") [string "Hello", id_ "World"]
                , expr $ call (id_ "fmt" /./ "Println") [string "Happy", id_ "Pi", string "Day"]
                , decl $ consts [ const' [id_ "Truth"] (Maybe Identifier `the` Nothing) [bool True] ]
                , expr $ call (id_ "fmt" /./ "Println") [string "Go rules?", id_ "Truth"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

