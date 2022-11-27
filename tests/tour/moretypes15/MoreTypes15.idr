module MoreTypes15

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "append.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars [ var [ id' "s" ] (array' $ id' "int") [] ]
                , expr $ call (id' "printSlice") [id' "s"]
                , [id' "s"] /=/ [ call (id' "append") [ id' "s", int 0 ] ]
                  |> doc " append works on nil slices."
                , expr $ call (id' "printSlice") [id' "s"]
                , [id' "s"] /=/ [ call (id' "append") [ id' "s", int 1 ] ]
                  |> doc " The slice grows as needed."
                , expr $ call (id' "printSlice") [id' "s"]
                , [id' "s"] /=/ [ call (id' "append") [ id' "s", int 2, int 3, int 4 ] ]
                  |> doc " We can add more element at a time."
                , expr $ call (id' "printSlice") [id' "s"]
                ]
              , func (id' "printSlice") [field ["s"] $ array' $ id' "int"] void
                [ expr $ call (id' "fmt" /./ "Printf")
                  [ string "len=%d cap=%d %v\\n"
                  , call (id' "len") [id' "s"]
                  , call (id' "cap") [id' "s"]
                  , id' "s"
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

