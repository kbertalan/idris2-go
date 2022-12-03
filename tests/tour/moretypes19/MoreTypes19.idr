module MoreTypes19

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "maps.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field ["Lat", "Long"] $ id_ "float64"
                  ]
                ]
              , vars
                [ var [id_ "m"] (map_ (id_ "string") (id_ "Vertex")) []
                ]
              , func (id_ "main") [] void
                [ [ id_ "m" ] /=/ [ call (id_ "make") [ map_ (id_ "string") (id_ "Vertex")] ]
                , [ id_ "m" `index` string "Bell Labs" ] /=/ [ composite (id_ "Vertex") [ float 40.68433, float (-74.39967) ] ]
                , expr $ call (id_ "fmt" /./ "Println") [ id_ "m" `index` string "Bell Labs" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

