module Basics10

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "short-variable-declarations.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars
                  [ var (map id' ["i", "j"]) (id' "int") [int 1, int 2]
                  ]
                , [id' "k"] /:=/ [int 3]
                , decl $ vars
                  [ var' (map id' ["c", "python", "java"]) [bool True, bool False, string "no!"]
                    ]
                , expr $ call (id' "fmt" /./ "Println") [id' "i", id' "j", id' "k", id' "c", id' "python", id' "java"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

