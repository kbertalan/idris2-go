module Basics15

import Data.List1
import Control.Monad.Either
import Go.AST.Printer as Go
import Go.AST.Combinators as Go
import System.File

main : IO ()
main = do
  let src = file "constants"
              (package "main")
              [ import' "fmt" ]
              [ consts [ const' [identifier "Pi"] (Maybe Identifier `the` Nothing) [float 3.14] ]
              , func (identifier "main") [] void
                [ decl $ consts [ const' [identifier "World"] (Maybe Identifier `the` Nothing) [string "世界"] ]
                , expr $ call (identifier "fmt" /./ identifier "Println") [string "Hello", identifier "World"]
                , expr $ call (identifier "fmt" /./ identifier "Println") [string "Happy", identifier "Pi", string "Day"]
                , decl $ consts [ const' [identifier "Truth"] (Maybe Identifier `the` Nothing) [bool True] ]
                , expr $ call (identifier "fmt" /./ identifier "Println") [string "Go rules?", identifier "Truth"]
                ]
              ]
  putStrLn "printing source:\n"
  Right () <- runEitherT $ Go.print stdout src
    | Left e => putStrLn $ show e
  pure ()

