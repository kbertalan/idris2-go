module Basics4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "functions.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id_ "add") [field ["x"] int, field ["y"] int] [field [] int]
                [ return [ id_ "x" /+/ id_ "y" ] ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println")
                  [ call (id_ "add") [intL 42, intL 13]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

