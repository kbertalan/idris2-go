module MoreTypes8

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices-pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "names" ] /:=/ [ compositL (array (intL 4) string) 
                    [ stringL "John"
                    , stringL "Paul"
                    , stringL "George"
                    , stringL "Ringo"
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "names" ]
                , [id_ "a"] /:=/ [ sliceLH (id_ "names") (intL 0) (intL 2) ]
                , [id_ "b"] /:=/ [ sliceLH (id_ "names") (intL 1) (intL 3) ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a", id_ "b" ]
                , [ id_ "b" `index` intL 0 ] /=/ [stringL "XXX"]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a", id_ "b" ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "names" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

