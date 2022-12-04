module MoreTypes14

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "slices-of-slice.go"
              (package "main")
              [ import' "fmt"
              , import' "strings"
              ]
              [ func (id_ "main") [] void
                [
                  [ id_ "board" ] /:=/
                  [ composit (array' $ array' (tid' "string"))
                    [ composit (array' $ tid' "string") $ [ string "_", string "_", string "_" ]
                    , composit (array' $ tid' "string") $ [ string "_", string "_", string "_" ]
                    , composit (array' $ tid' "string") $ [ string "_", string "_", string "_" ]
                    ]
                  ]
                  |> doc " Create a tic-tac-toe board."
                , [ ((id_ "board") `index` int 0) `index` int 0 ] /=/ [ string "X" ]
                  |> doc " The players take turns."
                , [ ((id_ "board") `index` int 2) `index` int 2 ] /=/ [ string "O" ]
                , [ ((id_ "board") `index` int 1) `index` int 2 ] /=/ [ string "X" ]
                , [ ((id_ "board") `index` int 1) `index` int 0 ] /=/ [ string "O" ]
                , [ ((id_ "board") `index` int 0) `index` int 2 ] /=/ [ string "X" ]
                , for_ ([id_ "i"] /:=/ [int 0]) (id_ "i" /</ call (id_ "len") [id_ "board"]) (inc $ id_ "i")
                  [ expr $ call (id_ "fmt" /./ "Printf")
                    [ string "%s\\n"
                    , call (id_ "strings" /./ "Join") [ id_ "board" `index` id_ "i", string " " ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

