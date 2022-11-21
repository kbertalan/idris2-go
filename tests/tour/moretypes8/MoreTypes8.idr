module MoreTypes8

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices-pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "names" ] /:=/ [ composite (array (int 4) (id' "string")) 
                    [ string "John"
                    , string "Paul"
                    , string "George"
                    , string "Ringo"
                    ]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "names" ]
                , [id' "a"] /:=/ [ sliceLH (id' "names") (int 0) (int 2) ]
                , [id' "b"] /:=/ [ sliceLH (id' "names") (int 1) (int 3) ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "a", id' "b" ]
                , [ id' "b" `index` int 0 ] /=/ [string "XXX"]
                , expr $ call (id' "fmt" /./ "Println") [ id' "a", id' "b" ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "names" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

