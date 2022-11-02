module Basics11

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt"
              , import' "math/cmplx"
              ]
              [ vars
                [ var (singleton $ identifier "ToBe") (Just $ identifier "bool") [bool False]
                , var (singleton $ identifier "MaxInt") (Just $ identifier "uint64") [int 1 /<</ int 64 /-/ int 1]
                , var (singleton $ identifier "z") (Just $ identifier "complex128") [call (identifier "cmplx" /./ identifier "Sqrt") [int (-5) /+/ imag 12]]
                ]
              , func (identifier "main") [] void
                [ expr $ call (identifier "fmt" /./ identifier "Printf") [string "Type: %T Value: %v\\n", identifier "ToBe", identifier "ToBe"]
                , expr $ call (identifier "fmt" /./ identifier "Printf") [string "Type: %T Value: %v\\n", identifier "MaxInt", identifier "MaxInt"]
                , expr $ call (identifier "fmt" /./ identifier "Printf") [string "Type: %T Value: %v\\n", identifier "z", identifier "z"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

