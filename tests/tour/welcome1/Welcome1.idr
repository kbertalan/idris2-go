module Welcome1

import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "hello.go"
              (package "main")
              [import' "fmt"]
              [ func (id' "main") [] void [
                expr $ call (id' "fmt" /./ "Println") [string "Hello,   "]
              ]]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

