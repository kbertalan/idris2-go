module MoreTypes13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "make-slices.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [id' "a"] /:=/ [call (id' "make") [array' $ id' "int", int 5]]
                , expr $ call (id' "printSlice") [string "a", id' "a"]
                , [id' "b"] /:=/ [call (id' "make") [array' $ id' "int", int 0, int 5]]
                , expr $ call (id' "printSlice") [string "b", id' "b"]
                , [id' "c"] /:=/ [id' "b" `sliceH` int 2]
                , expr $ call (id' "printSlice") [string "c", id' "c"]
                , [id' "d"] /:=/ [sliceLH (id' "c") (int 2) (int 5)]
                , expr $ call (id' "printSlice") [string "d", id' "d"]
                ]
              , func (id' "printSlice") [field ["s"] $ id' "string", field ["x"] $ array' $ id' "int"] void
                [ expr $ call (id' "fmt" /./ "Printf")
                  [ string "%s len=%d cap=%d %v\\n"
                  , id' "s"
                  , call (id' "len") [id' "x"]
                  , call (id' "cap") [id' "x"]
                  , id' "x"
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

