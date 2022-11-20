module MoreTypes11

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "slice-len-cap.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ [ id' "s" ] /:=/ [ composite (array' (id' "int")) 
                    [ int 2
                    , int 3
                    , int 5
                    , int 7
                    , int 11
                    , int 13
                    ]
                  ]
                , [ id' "s" ] /=/ [ sliceH (id' "s") (int 0)]
                  |> doc " Slice the slice to give it zero length."
                , expr $ call (id' "printSlice") [ id' "s" ]
                , [ id' "s" ] /=/ [ sliceH (id' "s") (int 4)]
                  |> doc " Extend its length."
                , expr $ call (id' "printSlice") [ id' "s" ]
                , [ id' "s" ] /=/ [ sliceL (id' "s") (int 2)]
                  |> doc " Drop its first two values."
                , expr $ call (id' "printSlice") [ id' "s" ]
                ]
              , func (id' "printSlice") [field ["s"] $ array' $ id' "int"] void
                [ expr $ call (id' "fmt" /./ "Printf")
                  [ string "len=%d cap=%d %v\\n"
                  , call (id' "len") [id' "s"]
                  , call (id' "cap") [id' "s"]
                  , id' "s"
                  ]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

