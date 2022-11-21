module MoreTypes7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "primes" ] /:=/ [ composite (array (int 6) (id' "int")) [int 2, int 3, int 5, int 7, int 11, int 13] ]
                , decl $ vars [ var [id' "s"] (array' $ id' "int") [ sliceLH (id' "primes") (int 1) (int 4) ] ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "s" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

