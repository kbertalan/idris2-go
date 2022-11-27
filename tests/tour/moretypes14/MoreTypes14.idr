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
              [ func (id' "main") [] void
                [
                  [ id' "board" ] /:=/
                  [ composite (array' $ array' (id' "string"))
                    [ composite (array' $ id' "string") $ [ string "_", string "_", string "_" ]
                    , composite (array' $ id' "string") $ [ string "_", string "_", string "_" ]
                    , composite (array' $ id' "string") $ [ string "_", string "_", string "_" ]
                    ]
                  ]
                  |> doc " Create a tic-tac-toe board."
                , [ ((id' "board") `index` int 0) `index` int 0 ] /=/ [ string "X" ]
                  |> doc " The players take turns."
                , [ ((id' "board") `index` int 2) `index` int 2 ] /=/ [ string "O" ]
                , [ ((id' "board") `index` int 1) `index` int 2 ] /=/ [ string "X" ]
                , [ ((id' "board") `index` int 1) `index` int 0 ] /=/ [ string "O" ]
                , [ ((id' "board") `index` int 0) `index` int 2 ] /=/ [ string "X" ]
                , for' ([id' "i"] /:=/ [int 0]) (id' "i" /</ call (id' "len") [id' "board"]) (inc $ id' "i")
                  [ expr $ call (id' "fmt" /./ "Printf")
                    [ string "%s\\n"
                    , call (id' "strings" /./ "Join") [ id' "board" `index` id' "i", string " " ]
                    ]
                  ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

