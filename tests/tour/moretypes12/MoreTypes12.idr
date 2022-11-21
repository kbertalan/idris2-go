module MoreTypes12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "nil-slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars [ var [ id' "s" ] (array' (id' "int")) [] ]
                , expr $ call (id' "fmt" /./ "Println")
                  [ id' "s"
                  , call (id' "len") [id' "s"]
                  , call (id' "cap") [id' "s"]
                  ]
                , if' (id' "s" /==/ id' "nil")
                  [ expr $ call (id' "fmt" /./ "Println") [ string "nil!" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

