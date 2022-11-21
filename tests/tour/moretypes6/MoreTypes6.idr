module MoreTypes6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars [ var [id' "a"] (array (int 2) $ id' "string") [] ]
                , [ id' "a" `index` int 0 ] /=/ [ string "Hello" ]
                , [ id' "a" `index` int 1 ] /=/ [ string "World" ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "a" `index` int 0, id' "a" `index` int 1]
                , expr $ call (id' "fmt" /./ "Println") [ id' "a" ]
                , [ id' "primes" ] /:=/ [ composite (array (int 6) (id' "int")) [int 2, int 3, int 5, int 7, int 11, int 13] ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "primes" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

