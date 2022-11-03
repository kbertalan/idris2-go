module Basics16

import Data.List1
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
                [ const' [identifier "Big"] (Maybe Identifier `the` Nothing) [int 1 /<</ int 100]
                  |> docs [ " Create a huge number by shifting a 1 bit left 100 places."
                          , " In other words, the binary number that is 1 followed by 100 zeroes."
                          ]
                , const' [identifier "Small"] (Maybe Identifier `the` Nothing) [identifier "Big" />>/ int 99]
                  |> doc " Shift it right again 99 places, so we end up with 1<<1, or 2."
                ]
              , func (identifier "needInt") [param "x" (identifier "int")] [type $ identifier "int"]
                [ return [ identifier "x" /*/ int 10 /+/ int 1 ] ]
              , func (identifier "needFloat") [param "x" (identifier "float64")] [type $ identifier "float64"]
                [ return [ identifier "x" /*/ float 0.1 ] ]
              , func (identifier "main") [] void
                [ expr $ call (identifier "fmt" /./ identifier "Println") [call (identifier "needInt") [identifier "Small"]]
                , expr $ call (identifier "fmt" /./ identifier "Println") [call (identifier "needFloat") [identifier "Small"]]
                , expr $ call (identifier "fmt" /./ identifier "Println") [call (identifier "needFloat") [identifier "Big"]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

