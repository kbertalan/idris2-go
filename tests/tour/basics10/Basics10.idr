module Basics10

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

fromList' : ( ls : List a ) -> { auto 0 ok : NonEmpty ls } -> List1 a
fromList' (x::xs) = x ::: xs

main : IO ()
main = do
  let src = file "short-variable-declarations.go"
              (package "main")
              [ import' "fmt" ]
              [ func (identifier "main") [] void
                [ decl $ vars
                  [ var (fromList' $ map identifier ["i", "j"]) (Just $ identifier "int") [int 1, int 2]
                  ]
                , [identifier "k"] /:=/ [int 3]
                , decl $ vars
                  [ var (fromList' $ map identifier ["c", "python", "java"]) (Maybe Identifier `the` Nothing) [bool True, bool False, string "no!"]
                    ]
                , expr $ call (identifier "fmt" /./ identifier "Println") [identifier "i", identifier "j", identifier "k", identifier "c", identifier "python", identifier "java"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

