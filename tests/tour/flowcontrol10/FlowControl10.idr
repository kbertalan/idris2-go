module FlowControl10

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "switch-evaulation-order.go"
              (package "main")
              [ import' "fmt"
              , import' "time"
              ]
              [ func (id' "main") [] void
                [ expr $ call (id' "fmt" /./ "Println") [ string "When's Sunday?" ]
                , [ id' "today" ] /:=/ [ call (call (id' "time" /./ "Now") [] /./ "Weekday") [] ]
                , switch (id' "time" /./ "Saturday")
                  [ case' [id' "today" /+/ int 0]
                    [ expr $ call (id' "fmt" /./ "Println") [string "Today."] ]
                  , case' [id' "today" /+/ int 1]
                    [ expr $ call (id' "fmt" /./ "Println") [string "Tomorrow."] ]
                  , case' [id' "today" /+/ int 2]
                    [ expr $ call (id' "fmt" /./ "Println") [string "In two days."] ]
                  , default'
                    [ expr (call (id' "fmt" /./ "Println") [string "Too far away."]) ]
                  ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

