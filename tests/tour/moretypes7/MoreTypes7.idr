module MoreTypes7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "primes" ] /:=/ [ compositL (array (intL 6) int) [intL 2, intL 3, intL 5, intL 7, intL 11, intL 13] ]
                , decl $ vars [ var [id_ "s"] (array' int) [ sliceLH (id_ "primes") (intL 1) (intL 4) ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

