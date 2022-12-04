module MoreTypes9

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-literals.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [ id_ "q" ] /:=/ [ compositL (array' int) 
                    [ intL 2
                    , intL 3
                    , intL 5
                    , intL 7
                    , intL 11
                    , intL 13
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "q" ]
                , [ id_ "r" ] /:=/ [ compositL (array' bool) 
                    [ boolL True
                    , boolL False
                    , boolL True
                    , boolL True
                    , boolL False
                    , boolL True
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "r" ]
                , [ id_ "s" ] /:=/ [ compositL (array' $ struct [ field "i" int, field "b" bool ])
                    [ compositL' [intL 2, boolL True]
                    , compositL' [intL 3, boolL False]
                    , compositL' [intL 5, boolL True]
                    , compositL' [intL 7, boolL True]
                    , compositL' [intL 11, boolL False]
                    , compositL' [intL 13, boolL True]
                    ]
                  ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

