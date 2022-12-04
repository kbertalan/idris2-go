module MoreTypes20

import Go.AST.Printer as Go
import Go.AST.Combinators as Go

main : IO ()
main = do
  let src = file "map-literals.go"
              (package "main")
              [ import' "fmt" ]
              [ types [ type "Vertex" [] $ struct [ field ["Lat", "Long"] float64 ] ]
              , vars
                [ var' [id_ "m"]
                  [ compositL (map_ string (tid' "Vertex"))
                    [ stringL "Bell Labs" /:/ compositL (tid' "Vertex") [ floatL 40.68433, floatL (-74.39967) ]
                    , stringL "Google" /:/ compositL (tid' "Vertex") [ floatL 37.42202, floatL (-122.08408) ]
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

