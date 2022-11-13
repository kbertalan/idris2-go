module Basics8

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "variables.go"
              (package "main")
              [ import' "fmt" ]
              [ vars
                [ var (map id' ["c", "python", "java"]) (id' "bool") []
                ]
              , func (id' "main") [] void
                [ decl $ vars [ var [id' "i"] (id' "int") [] ]
                , expr $ call (id' "fmt" /./ "Println") [id' "i", id' "c", id' "python", id' "java"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

