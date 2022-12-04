module MoreTypes11

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slice-len-cap.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "s" ] /:=/ [ compositL (array' int) 
                    [ intL 2
                    , intL 3
                    , intL 5
                    , intL 7
                    , intL 11
                    , intL 13
                    ]
                  ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (intL 0)]
                  |> doc " Slice the slice to give it zero length."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceH (id_ "s") (intL 4)]
                  |> doc " Extend its length."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
                , [ id_ "s" ] /=/ [ sliceL (id_ "s") (intL 2)]
                  |> doc " Drop its first two values."
                , expr $ call (id_ "printSlice") [ id_ "s" ]
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

