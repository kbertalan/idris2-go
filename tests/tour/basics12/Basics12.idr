module Basics12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [id_ "i"] (id_ "int") [] ]
                , decl $ vars [ var [id_ "f"] (id_ "float64") [] ]
                , decl $ vars [ var [id_ "b"] (id_ "bool") [] ]
                , decl $ vars [ var [id_ "s"] (id_ "string") [] ]
                , expr $ call (id_ "fmt" /./ "Printf") [string "%v %v %v %q\\n", id_ "i", id_ "f", id_ "b", id_ "s"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

