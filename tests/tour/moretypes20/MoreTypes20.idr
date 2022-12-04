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
                  [ field ["Lat", "Long"] $ tid' "float64"
                  ]
                ]
              , vars
                [ var' [id_ "m"]
                  [ composit (map_ (tid' "string") (tid' "Vertex"))
                    [ string "Bell Labs" /:/ composit (tid' "Vertex") [ float 40.68433, float (-74.39967) ]
                    , string "Google" /:/ composit (tid' "Vertex") [ float 37.42202, float (-122.08408) ]
                    ]
                  ]
                ]
              , func (id_ "main") [] void
                [ expr $ call (id_ "fmt" /./ "Println") [ id_ "m" ]
                ]
              ]

  Right () <- printFile "build/go" src
    | Left e => putStrLn $ show e
  pure ()

