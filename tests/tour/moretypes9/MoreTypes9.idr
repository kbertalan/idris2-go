module MoreTypes9

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "q" ] /:=/ [ composit (array' (tid' "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "q" ]
                , [ id_ "r" ] /:=/ [ composit (array' (tid' "bool")) 
                    [ bool True
                    , bool False
                    , bool True
                    , bool True
                    , bool False
                    , bool True
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "r" ]
                , [ id_ "s" ] /:=/ [ composit (array' $ struct [ field ["i"] $ tid' "int", field ["b"] $ tid' "bool" ])
                    [ composit' [int 2, bool True]
                    , composit' [int 3, bool False]
                    , composit' [int 5, bool True]
                    , composit' [int 7, bool True]
                    , composit' [int 11, bool False]
                    , composit' [int 13, bool True]
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

