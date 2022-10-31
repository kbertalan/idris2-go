module Basics8

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

fromList' : ( ls : List a ) -> { auto 0 ok : NonEmpty ls } -> List1 a
fromList' (x::xs) = x ::: xs

main : IO ()
main = do
  let src = file "variables.go"
              (package "main")
              [ import' "fmt" ]
              [ vars
                [ var (fromList' $ map identifier ["c", "python", "java"]) (Just $ identifier "bool") []
                ]
              , func (identifier "main") [] void
                [ decl $ vars [ var (identifier "i" ::: Nil) (Just $ identifier "int") [] ]
                , expr $ call (identifier "fmt" /./ identifier "Println") [identifier "i", identifier "c", identifier "python", identifier "java"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

