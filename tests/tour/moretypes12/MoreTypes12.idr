module MoreTypes12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "nil-slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [ id_ "s" ] (array' (id_ "int")) [] ]
                , expr $ call (id_ "fmt" /./ "Println")
                  [ id_ "s"
                  , call (id_ "len") [id_ "s"]
                  , call (id_ "cap") [id_ "s"]
                  ]
                , if_ (id_ "s" /==/ id_ "nil")
                  [ expr $ call (id_ "fmt" /./ "Println") [ string "nil!" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

