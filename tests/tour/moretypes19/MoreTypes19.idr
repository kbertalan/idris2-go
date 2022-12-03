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
                  [ field ["Lat", "Long"] $ id' "float64"
                  ]
                ]
              , vars
                [ var [id' "m"] (map' (id' "string") (id' "Vertex")) []
                ]
              , func (id' "main") [] void
                [ [ id' "m" ] /=/ [ call (id' "make") [ map' (id' "string") (id' "Vertex")] ]
                , [ id' "m" `index` string "Bell Labs" ] /=/ [ composite (id' "Vertex") [ float 40.68433, float (-74.39967) ] ]
                , expr $ call (id' "fmt" /./ "Println") [ id' "m" `index` string "Bell Labs" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

