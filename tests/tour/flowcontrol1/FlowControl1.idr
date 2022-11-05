module FlowControl1

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "for.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [id' "sum"] /:=/ [int 0]
                , for' ([id' "i"] /:=/ [int 0]) (id' "i" /</ int 10) (inc $ id' "i")
                  [ [id' "sum" ] /+=/ [id' "i"] ]
                , expr $ call (id' "fmt" /./ "Println") [id' "sum"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

