module MoreTypes19

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "maps.go"
              (package "main")
              [ import_ "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ fields ["Lat", "Long"] $ float64
                  ]
                ]
              , vars
                [ var [id_ "m"] (map_ string (tid' "Vertex")) []
                ]
              , func "main" [] void
                [ [ id_ "m" ] /=/ [make (map_ string (tid' "Vertex")) [] ]
                , [ id_ "m" `index` stringL "Bell Labs" ] /=/ [ compositL (tid' "Vertex") [ floatL 40.68433, floatL (-74.39967) ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "m" `index` stringL "Bell Labs" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

