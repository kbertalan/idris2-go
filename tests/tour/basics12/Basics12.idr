module Basics12

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "basic-types.go"
              (package "main")
              [ import' "fmt" ]
              [ func (id' "main") [] void
                [ decl $ vars [ var [id' "i"] (id' "int") [] ]
                , decl $ vars [ var [id' "f"] (id' "float64") [] ]
                , decl $ vars [ var [id' "b"] (id' "bool") [] ]
                , decl $ vars [ var [id' "s"] (id' "string") [] ]
                , expr $ call (id' "fmt" /./ "Printf") [string "%v %v %v %q\\n", id' "i", id' "f", id' "b", id' "s"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

