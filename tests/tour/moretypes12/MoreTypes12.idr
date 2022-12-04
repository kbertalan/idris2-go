module MoreTypes12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "nil-slices.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ decl $ vars [ var [ id_ "s" ] (array' int) [] ]
                , expr $ call (id_ "fmt" /./ "Println")
                  [ id_ "s"
                  , call (id_ "len") [id_ "s"]
                  , call (id_ "cap") [id_ "s"]
                  ]
                , if_ (id_ "s" /==/ id_ "nil")
                  [ expr $ call (id_ "fmt" /./ "Println") [ stringL "nil!" ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

