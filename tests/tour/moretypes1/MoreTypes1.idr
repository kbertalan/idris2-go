module MoreTypes1

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "pointers.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "i", id' "j" ] /:=/ [int 42, int 2701]
                , [ id' "p" ] /:=/ [ /&/ id' "i" ]
                  |> comment " point to i"
                , expr (call (id' "fmt" /./ "Println") [ star $ id' "p" ])
                  |> comment " read i through the pointer"
                , [ star $ id' "p" ] /=/ [ int 21 ]
                  |> comment " set i through the pointer"
                , expr (call (id' "fmt" /./ "Println") [id' "i"])
                  |> comment " see the new value of i"
                , [ id' "p" ] /=/ [ /&/ id' "j" ]
                  |> comment " point to j"
                , [ star $ id' "p" ] /=/ [ star (id' "p") /// int 37 ]
                  |> comment " divide j through the pointer"
                , expr (call (id' "fmt" /./ "Println") [ id' "j" ])
                  |> comment " see the new value of j"
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

