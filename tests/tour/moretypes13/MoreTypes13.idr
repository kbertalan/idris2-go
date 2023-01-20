module MoreTypes13

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "make-slices.go"
              (package "main")
              [ import_ "fmt" ]
              [ func "main" [] void
                [ [id_ "a"] /:=/ [make (array' int) [intL 5]]
                , expr $ call (id_ "printSlice") [stringL "a", id_ "a"]
                , [id_ "b"] /:=/ [make (array' int) [intL 0, intL 5]]
                , expr $ call (id_ "printSlice") [stringL "b", id_ "b"]
                , [id_ "c"] /:=/ [id_ "b" `sliceH` intL 2]
                , expr $ call (id_ "printSlice") [stringL "c", id_ "c"]
                , [id_ "d"] /:=/ [sliceLH (id_ "c") (intL 2) (intL 5)]
                , expr $ call (id_ "printSlice") [stringL "d", id_ "d"]
                ]
              , func "printSlice" [field "s" $ string, field "x" $ array' int] void
                [ expr $ call (id_ "fmt" /./ "Printf")
                  [ stringL "%s len=%d cap=%d %v\n"
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

