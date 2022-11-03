module Basics5

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "functions-continued.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id' "add") [field ["x", "y"] (id' "int")] [field [] $ id' "int"]
                [ return [ id' "x" /+/ id' "y" ] ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println")
                  [ call (id' "add") [int 42, int 13]
                  ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

