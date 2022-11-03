module Basics1

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "packages.go"
              (package "main")
              [ import' "fmt"
              , import' "math/rand"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "My favorite number is", call (id' "rand" /./ "Intn") [int 10]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

