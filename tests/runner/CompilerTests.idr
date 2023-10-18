module CompilerTests

import Idris2Command

import Test.Golden.RunnerHelper

main : IO ()
main = do
  _ <- MkRunScriptArg . const <$> idris2Command "idris2-go"
  goldenRunner [ "Compile to Go" `atDir` "go" ]

