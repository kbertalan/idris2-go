module Basics6

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "multiple-results.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id' "swap") [field ["x", "y"] $ id' "string"] [field [] $ id' "string", field [] $ id' "string"]
                [ return [ id' "y" , id' "x" ] ]
              , func (id' "main") [] void
                [ [id' "a", id' "b"] /:=/ [call (id' "swap") [string "hello", string "world"]]
                , expr $ call (id' "fmt" /./ "Println") [ id' "a", id' "b" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

