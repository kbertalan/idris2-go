module FlowControl7

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "if-and-else.go"
              (package "main")
              [ import' "fmt"
              , import' "math"
              ]
              [ func (id' "pow") [field ["x","n","lim"] $ id' "float64"] [field [] $ id' "float64"]
                [ ifsE ([id' "v"] /:=/ [call (id' "math" /./ "Pow") [id' "x", id' "n"]]) (id' "v" /</ id' "lim")
                  [ return [ id' "v" ] ]
                  $ block [ expr $ call (id' "fmt" /./ "Printf") [string "%g >= %g \\n", id' "v", id' "lim"] ]
                , return [ id' "lim" ]
                  |> doc " can't use v here, though"
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

