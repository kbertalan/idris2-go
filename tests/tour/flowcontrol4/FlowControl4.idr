module FlowControl4

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "forever.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ forever []
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

