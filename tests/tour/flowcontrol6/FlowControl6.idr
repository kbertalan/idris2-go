module FlowControl6

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "if-with-a-short-statement.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id' "pow") [field ["x","n","lim"] $ id' "float64"] [field [] $ id' "float64"]
                [ ifs ([id' "v"] /:=/ [call (id' "math" /./ "Pow") [id' "x", id' "n"]]) (id' "v" /</ id' "lim")
                  [ return [ id' "v" ] ]
                , return [ id' "lim" ]
                ]
              , func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println")
                  [ call (id' "pow") [int 3, int 2, int 10]
                  , call (id' "pow") [int 3, int 3, int 20]
                  ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

