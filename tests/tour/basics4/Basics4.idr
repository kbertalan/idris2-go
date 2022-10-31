module Basics4

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "functions.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (identifier "add") [param "x" (identifier "int"), param "y" (identifier "int")] [type $ identifier "int"]
                [ return [ identifier "x" /+/ identifier "y" ] ]
              , func (identifier "main") [] void
                [ expr $ call (identifier "fmt" /./ identifier "Println")
                  [ call (identifier "add") [int 42, int 13]
                  ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

