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
                func (id' "split") [field ["sum"] $ id' "int"] [field ["x", "y"] $ id' "int"]
                [ [id' "x"] /=/ [id' "sum" /*/ int 4 /// int 9]
                , [id' "y"] /=/ [id' "sum" /-/ id' "x"]
                , return []
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [call (id' "split") [int 17]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

