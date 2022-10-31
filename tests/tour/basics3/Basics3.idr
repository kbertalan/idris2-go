module Basics3

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "exported-names.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [func (identifier "main") [] void 
                [ expr $ call (identifier "fmt" /./ identifier "Println") [identifier "math" /./ identifier "Pi"] ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()
