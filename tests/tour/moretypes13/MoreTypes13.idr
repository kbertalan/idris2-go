module MoreTypes13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "make-slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [id_ "a"] /:=/ [make (array' $ tid' "int") [int 5]]
                , expr $ call (id_ "printSlice") [string "a", id_ "a"]
                , [id_ "b"] /:=/ [make (array' $ tid' "int") [int 0, int 5]]
                , expr $ call (id_ "printSlice") [string "b", id_ "b"]
                , [id_ "c"] /:=/ [id_ "b" `sliceH` int 2]
                , expr $ call (id_ "printSlice") [string "c", id_ "c"]
                , [id_ "d"] /:=/ [sliceLH (id_ "c") (int 2) (int 5)]
                , expr $ call (id_ "printSlice") [string "d", id_ "d"]
                ]
              , func (id_ "printSlice") [field ["s"] $ tid' "string", field ["x"] $ array' $ tid' "int"] void
                [ expr $ call (id_ "fmt" /./ "Printf")
                  [ string "%s len=%d cap=%d %v\\n"
                  , id_ "s"
                  , call (id_ "len") [id_ "x"]
                  , call (id_ "cap") [id_ "x"]
                  , id_ "x"
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

