module Basics6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "multiple-results.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id_ "swap") [field ["x", "y"] string] [field [] string, field [] string]
                [ return [ id_ "y" , id_ "x" ] ]
              , func (id_ "main") [] void
                [ [id_ "a", id_ "b"] /:=/ [call (id_ "swap") [stringL "hello", stringL "world"]]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "a", id_ "b" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

