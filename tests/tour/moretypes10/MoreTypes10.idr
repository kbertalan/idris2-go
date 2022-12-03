module MoreTypes10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-bounds.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "s" ] /:=/ [ composite (array' (id_ "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , [ id_ "s" ] /=/ [ sliceLH (id_ "s") (int 1) (int 4)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (int 2)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceL (id_ "s") (int 1)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

