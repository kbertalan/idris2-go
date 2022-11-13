module MoreTypes9

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "slice-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "q" ] /:=/ [ composite (array' (id' "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "q" ]
                , [ id' "r" ] /:=/ [ composite (array' (id' "bool")) 
                    [ bool True
                    , bool False
                    , bool True
                    , bool True
                    , bool False
                    , bool True
                    ]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "r" ]
                , [ id' "s" ] /:=/ [ composite (array' $ struct [ field ["i"] $ id' "int", field ["b"] $ id' "bool" ])
                    [ composite' [int 2, bool True]
                    , composite' [int 3, bool False]
                    , composite' [int 5, bool True]
                    , composite' [int 7, bool True]
                    , composite' [int 11, bool False]
                    , composite' [int 13, bool True]
                    ]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "s" ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

