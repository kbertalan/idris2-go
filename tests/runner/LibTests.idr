module LibTests

import Idris2Command

import Test.Golden.RunnerHelper

main : IO ()
main = do
  _ <- MkRunScriptArg . const <$> idris2Command "idris2"
  goldenRunner [ "Tour of Go" `atDir` "tour" ]

