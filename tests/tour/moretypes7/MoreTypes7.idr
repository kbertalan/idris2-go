module MoreTypes7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "primes" ] /:=/ [ composit (array (int 6) (tid' "int")) [int 2, int 3, int 5, int 7, int 11, int 13] ]
                , decl $ vars [ var [id_ "s"] (array' $ tid' "int") [ sliceLH (id_ "primes") (int 1) (int 4) ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

