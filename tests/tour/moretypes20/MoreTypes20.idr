module MoreTypes20

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "map-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ types
                [ type "Vertex" [] $ struct
                  [ field ["Lat", "Long"] $ id' "float64"
                  ]
                ]
              , vars
                [ var' [id' "m"]
                  [ composite (map' (id' "string") (id' "Vertex"))
                    [ string "Bell Labs" /:/ composite (id' "Vertex") [ float 40.68433, float (-74.39967) ]
                    , string "Google" /:/ composite (id' "Vertex") [ float 37.42202, float (-122.08408) ]
                    ]
                  ]
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [ id' "m" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

