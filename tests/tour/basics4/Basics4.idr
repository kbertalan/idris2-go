module Basics4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "functions.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id_ "add") [field ["x"] $ id_ "int", field ["y"] $ id_ "int"] [field [] $ id_ "int"]
                [ return [ id_ "x" /+/ id_ "y" ] ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "add") [int 42, int 13]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

