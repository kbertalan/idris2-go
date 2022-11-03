module Basics13

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "type-conversions.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (identifier "main") [] void
                [ decl $ vars [ var (identifier "x" ::: [identifier "y"]) (Just $ identifier "int") [int 3, int 4] ]
                , decl $ vars [ var (singleton $ identifier "f") (Just $ identifier "float64") [
                    call (identifier "math" /./ identifier "Sqrt") [
                      call (identifier "float64") [ identifier "x" /*/ identifier "x" /+/ identifier "y" /*/ identifier "y" ]
                    ]
                  ] ]
                , decl $ vars [ var (singleton $ identifier "z") (Just $ identifier "uint") [
                    call (identifier "uint") [identifier "f"]
                  ] ]
                , expr $ call (identifier "fmt" /./ identifier "Println") [identifier "x", identifier "y", identifier "z"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

