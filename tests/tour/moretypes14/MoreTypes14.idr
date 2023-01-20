module MoreTypes14

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices-of-slice.go"
              (package "main")
              [ import_ "fmt"
              , import_ "strings"
              ]
              [ func "main" [] void
                [
                  [ id_ "board" ] /:=/
                  [ compositL (array' $ array' string)
                    [ compositL (array' $ string) $ [ stringL "_", stringL "_", stringL "_" ]
                    , compositL (array' $ string) $ [ stringL "_", stringL "_", stringL "_" ]
                    , compositL (array' $ string) $ [ stringL "_", stringL "_", stringL "_" ]
                    ]
                  ]
                  |> doc " Create a tic-tac-toe board."
                , [ ((id_ "board") `index` intL 0) `index` intL 0 ] /=/ [ stringL "X" ]
                  |> doc " The players take turns."
                , [ ((id_ "board") `index` intL 2) `index` intL 2 ] /=/ [ stringL "O" ]
                , [ ((id_ "board") `index` intL 1) `index` intL 2 ] /=/ [ stringL "X" ]
                , [ ((id_ "board") `index` intL 1) `index` intL 0 ] /=/ [ stringL "O" ]
                , [ ((id_ "board") `index` intL 0) `index` intL 2 ] /=/ [ stringL "X" ]
                , for_ ([id_ "i"] /:=/ [intL 0]) (id_ "i" /</ call (id_ "len") [id_ "board"]) (inc $ id_ "i")
                  [ expr $ call (id_ "fmt" /./ "Printf")
                    [ stringL "%s\n"
                    , call (id_ "strings" /./ "Join") [ id_ "board" `index` id_ "i", stringL " " ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

