module FlowControl3

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "for-is-gos-while.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [id' "sum"] /:=/ [int 1]
                , while (id' "sum" /</ int 1000)
                  [ [id' "sum" ] /+=/ [id' "sum"] ]
                , expr $ call (id' "fmt" /./ "Println") [id' "sum"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

