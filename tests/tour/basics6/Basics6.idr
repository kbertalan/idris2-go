module Basics6

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "multiple-results.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (identifier "swap") [params ["x", "y"] (identifier "string")] [type $ identifier "string", type $ identifier "string"]
                [ return [ identifier "y" , identifier "x" ] ]
              , func (identifier "main") [] void
                [ [identifier "a", identifier "b"] /:=/ [call (identifier "swap") [string "hello", string "world"]]
                , expr $ call (identifier "fmt" /./ identifier "Println") [ identifier "a", identifier "b" ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

