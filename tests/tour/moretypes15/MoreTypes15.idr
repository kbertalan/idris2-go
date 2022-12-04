module MoreTypes15

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "append.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [ id_ "s" ] (array' int) [] ]
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", intL 0 ] ]
                  |> doc " append works on nil slices."
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", intL 1 ] ]
                  |> doc " The slice grows as needed."
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", intL 2, intL 3, intL 4 ] ]
                  |> doc " We can add more element at a time."
                , expr $ call (id_ "printSlice") [id_ "s"]
                ]
              , func (id_ "printSlice") [field ["s"] $ array' int] void
                [ expr $ call (id_ "fmt" /./ "Printf")
                  [ stringL "len=%d cap=%d %v\\n"
                  , call (id_ "len") [id_ "s"]
                  , call (id_ "cap") [id_ "s"]
                  , id_ "s"
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

