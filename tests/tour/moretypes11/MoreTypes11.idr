module MoreTypes11

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-len-cap.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "s" ] /:=/ [ composite (array' (id_ "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (int 0)]
                  |> doc " Slice the slice to give it zero length."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (int 4)]
                  |> doc " Extend its length."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceL (id_ "s") (int 2)]
                  |> doc " Drop its first two values."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
                ]
              , func (id_ "printSlice") [field ["s"] $ array' $ id_ "int"] void
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

