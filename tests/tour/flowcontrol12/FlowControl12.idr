module FlowControl12

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "defer.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ defer $ call (id' "fmt" /./ "Println") [string "world"]
                , expr $ call (id' "fmt" /./ "Println") [string "hello"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

