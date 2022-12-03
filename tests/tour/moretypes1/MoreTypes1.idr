module MoreTypes1

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id_ "main") [] void
                [ [ id_ "i", id_ "j" ] /:=/ [int 42, int 2701]
                , [ id_ "p" ] /:=/ [ /&/ id_ "i" ]
                  |> comment " point to i"
                , expr (call (id_ "fmt" /./ "Println") [ star $ id_ "p" ])
                  |> comment " read i through the pointer"
                , [ star $ id_ "p" ] /=/ [ int 21 ]
                  |> comment " set i through the pointer"
                , expr (call (id_ "fmt" /./ "Println") [id_ "i"])
                  |> comment " see the new value of i"
                , [ id_ "p" ] /=/ [ /&/ id_ "j" ]
                  |> comment " point to j"
                , [ star $ id_ "p" ] /=/ [ star (id_ "p") /// int 37 ]
                  |> comment " divide j through the pointer"
                , expr (call (id_ "fmt" /./ "Println") [ id_ "j" ])
                  |> comment " see the new value of j"
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

