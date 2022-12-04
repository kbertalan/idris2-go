module MoreTypes10

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-bounds.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [ id_ "s" ] /:=/ [ compositL (array' int) 
                    [ intL 2
                    , intL 3
                    , intL 5
                    , intL 7
                    , intL 11
                    , intL 13
                    ]
                  ]
                , [ id_ "s" ] /=/ [ sliceLH (id_ "s") (intL 1) (intL 4)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (intL 2)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceL (id_ "s") (intL 1)]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

