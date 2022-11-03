module Welcome4

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "sandbox.go"
              (package "main")
              [ import' "fmt"
              , import' "time"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "Welcome to the playground!"]
                , expr $ call (id' "fmt" /./ "Println") [string "The time is", call (id' "time.Now") []]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

