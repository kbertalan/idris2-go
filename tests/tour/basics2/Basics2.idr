module Basics2

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "imports.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [func (identifier "main") [] void 
                [ expr $ call (identifier "fmt" /./ identifier "Printf") [string "Now you have %g problems.\\n", call (identifier "math" /./ identifier "Sqrt") [int 7]]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

