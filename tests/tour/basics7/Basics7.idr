module Basics7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "named-results.go"
              (package "main")
              [ import' "fmt" ]
              [
                func (id' "split") [field ["sum"] $ id' "int"] [field ["x", "y"] $ id' "int"]
                [ [id' "x"] /=/ [id' "sum" /*/ int 4 /// int 9]
                , [id' "y"] /=/ [id' "sum" /-/ id' "x"]
                , return []
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [call (id' "split") [int 17]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

