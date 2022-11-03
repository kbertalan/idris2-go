module Basics11

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
                [ var [id' "ToBe"] (Just $ id' "bool") [bool False]
                , var [id' "MaxInt"] (Just $ id' "uint64") [int 1 /<</ int 64 /-/ int 1]
                , var [id' "z"] (Just $ id' "complex128") [call (id' "cmplx" /./ "Sqrt") [int (-5) /+/ imag 12]]
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id' "ToBe", id' "ToBe"]
                , expr $ call (id' "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id' "MaxInt", id' "MaxInt"]
                , expr $ call (id' "fmt" /./ "Printf") [string "Type: %T Value: %v\\n", id' "z", id' "z"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

