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
              [func (identifier "main") [] void 
                [ expr $ call (identifier "fmt" /./ identifier "Println") [string "My favorite number is", call (identifier "rand" /./ identifier "Intn") [int 10]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

