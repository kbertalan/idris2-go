module Basics12

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [id_ "i"] (tid' "int") [] ]
                , decl $ vars [ var [id_ "f"] (tid' "float64") [] ]
                , decl $ vars [ var [id_ "b"] (tid' "bool") [] ]
                , decl $ vars [ var [id_ "s"] (tid' "string") [] ]
                , expr $ call (id_ "fmt" /./ "Printf") [string "%v %v %v %q\\n", id_ "i", id_ "f", id_ "b", id_ "s"]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

