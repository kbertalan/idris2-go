module MoreTypes10

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "slice-bounds.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "s" ] /:=/ [ composite (array' (id' "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , [ id' "s" ] /=/ [ sliceLH (id' "s") (int 1) (int 4)]
                , expr $ call (id' "fmt" /./ "Println") [ id' "s" ]
                , [ id' "s" ] /=/ [ sliceH (id' "s") (int 2)]
                , expr $ call (id' "fmt" /./ "Println") [ id' "s" ]
                , [ id' "s" ] /=/ [ sliceL (id' "s") (int 1)]
                , expr $ call (id' "fmt" /./ "Println") [ id' "s" ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

