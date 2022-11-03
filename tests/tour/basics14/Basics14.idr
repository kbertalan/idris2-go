module Basics14

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [id' "v"] /:=/ [int 42] |> comment " change me!"
                , expr $ call (id' "fmt" /./ "Printf") [string "v is of type %T\\n", id' "v"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

