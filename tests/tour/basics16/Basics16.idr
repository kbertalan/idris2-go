module Basics16

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "numeric-constants.go"
              (package "main")
              [ import' "fmt" ]
              [ consts
                [ const' [id' "Big"] (Maybe Identifier `the` Nothing) [int 1 /<</ int 100]
                  |> docs [ " Create a huge number by shifting a 1 bit left 100 places."
                          , " In other words, the binary number that is 1 followed by 100 zeroes."
                          ]
                , const' [id' "Small"] (Maybe Identifier `the` Nothing) [id' "Big" />>/ int 99]
                  |> doc " Shift it right again 99 places, so we end up with 1<<1, or 2."
                ]
              , func (id' "needInt") [field ["x"] (id' "int")] [field [] $ id' "int"]
                [ return [ id' "x" /*/ int 10 /+/ int 1 ] ]
              , func (id' "needFloat") [field ["x"] (id' "float64")] [field [] $ id' "float64"]
                [ return [ id' "x" /*/ float 0.1 ] ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [call (id' "needInt") [id' "Small"]]
                , expr $ call (id' "fmt" /./ "Println") [call (id' "needFloat") [id' "Small"]]
                , expr $ call (id' "fmt" /./ "Println") [call (id' "needFloat") [id' "Big"]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

