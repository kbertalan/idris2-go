module FlowControl13

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "defer-multi.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [string "counting"]
                , for' ([id' "i"] /:=/ [int 0]) (id' "i" /</ int 10) (inc $ id' "i")
                  [ defer $ call (id' "fmt" /./ "Println") [id' "i"]
                  ]
                , expr $ call (id' "fmt" /./ "Println") [string "done"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

