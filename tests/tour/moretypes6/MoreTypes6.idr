module MoreTypes6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [id_ "a"] (array (int 2) $ id_ "string") [] ]
                , [ id_ "a" `index` int 0 ] /=/ [ string "Hello" ]
                , [ id_ "a" `index` int 1 ] /=/ [ string "World" ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a" `index` int 0, id_ "a" `index` int 1]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a" ]
                , [ id_ "primes" ] /:=/ [ composite (array (int 6) (id_ "int")) [int 2, int 3, int 5, int 7, int 11, int 13] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "primes" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

