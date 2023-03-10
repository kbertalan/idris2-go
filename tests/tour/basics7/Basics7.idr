module Basics7

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "named-results.go"
              (package "main")
              [ import_ "fmt" ]
              [
                func "split" [field "sum" $ int] [fields ["x", "y"] int]
                [ [id_ "x"] /=/ [id_ "sum" /*/ intL 4 /// intL 9]
                , [id_ "y"] /=/ [id_ "sum" /-/ id_ "x"]
                , return []
                ]
              , func "main" [] void
                [ expr $ call (id_ "fmt" /./ "Println") [call (id_ "split") [intL 17]]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

