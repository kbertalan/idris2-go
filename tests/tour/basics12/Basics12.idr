module Basics12

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt" ]
              [ func (identifier "main") [] void
                [ decl $ vars [ var (singleton $ identifier "i") (Just $ identifier "int") [] ]
                , decl $ vars [ var (singleton $ identifier "f") (Just $ identifier "float64") [] ]
                , decl $ vars [ var (singleton $ identifier "b") (Just $ identifier "bool") [] ]
                , decl $ vars [ var (singleton $ identifier "s") (Just $ identifier "string") [] ]
                , expr $ call (identifier "fmt" /./ identifier "Printf") [string "%v %v %v %q\\n", identifier "i", identifier "f", identifier "b", identifier "s"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

