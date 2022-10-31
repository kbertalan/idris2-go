module Basics7

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "named-results.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (identifier "split") [param "sum" $ identifier "int"] [params ["x", "y"] (identifier "int")]
                [ [identifier "x"] /=/ [identifier "sum" /*/ int 4 /// int 9]
                , [identifier "y"] /=/ [identifier "sum" /-/ identifier "x"]
                , return []
                ]
              , func (identifier "main") [] void
                [ expr $ call (identifier "fmt" /./ identifier "Println") [call (identifier "split") [int 17]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

