module MoreTypes6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "struct-literals.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ decl $ vars [ var [id_ "a"] (array (intL 2) $ string) [] ]
                , [ id_ "a" `index` intL 0 ] /=/ [ stringL "Hello" ]
                , [ id_ "a" `index` intL 1 ] /=/ [ stringL "World" ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a" `index` intL 0, id_ "a" `index` intL 1]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a" ]
                , [ id_ "primes" ] /:=/ [ compositL (array (intL 6) int) [intL 2, intL 3, intL 5, intL 7, intL 11, intL 13] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "primes" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

