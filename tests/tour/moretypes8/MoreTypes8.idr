module MoreTypes8

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices-pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "names" ] /:=/ [ composit (array (int 4) (tid' "string")) 
                    [ string "John"
                    , string "Paul"
                    , string "George"
                    , string "Ringo"
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "names" ]
                , [id_ "a"] /:=/ [ sliceLH (id_ "names") (int 0) (int 2) ]
                , [id_ "b"] /:=/ [ sliceLH (id_ "names") (int 1) (int 3) ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a", id_ "b" ]
                , [ id_ "b" `index` int 0 ] /=/ [string "XXX"]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a", id_ "b" ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "names" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

