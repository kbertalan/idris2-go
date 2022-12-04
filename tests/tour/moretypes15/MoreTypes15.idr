module MoreTypes15

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "append.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ decl $ vars [ var [ id_ "s" ] (array' $ tid' "int") [] ]
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", int 0 ] ]
                  |> doc " append works on nil slices."
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", int 1 ] ]
                  |> doc " The slice grows as needed."
                , expr $ call (id_ "printSlice") [id_ "s"]
                , [id_ "s"] /=/ [ call (id_ "append") [ id_ "s", int 2, int 3, int 4 ] ]
                  |> doc " We can add more element at a time."
                , expr $ call (id_ "printSlice") [id_ "s"]
                ]
              , func (id_ "printSlice") [field ["s"] $ array' $ tid' "int"] void
                [ expr $ call (id_ "fmt" /./ "Printf")
                  [ string "len=%d cap=%d %v\\n"
                  , call (id_ "len") [id_ "s"]
                  , call (id_ "cap") [id_ "s"]
                  , id_ "s"
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

