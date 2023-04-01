module LibTests

import BaseDir
import Idris2Command

import Test.Golden.RunnerHelper

main : IO ()
main = do
  _ <- MkRunScriptArg <$> idris2Command "idris2"
  goldenRunner [ "Tour of Go" `atDir` "tour" ]

