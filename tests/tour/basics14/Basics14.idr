module Basics14

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import' "fmt" ]
              [ func (identifier "main") [] void
                [ [identifier "v"] /:=/ [int 42] |> comment " change me!"
                , expr $ call (identifier "fmt" /./ identifier "Printf") [string "v is of type %T\\n", identifier "v"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

