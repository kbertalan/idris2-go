module Main

import Test.Golden

tourOfGo : TestPool
tourOfGo = MkTestPool "Tour of Go" [] Nothing
  [ "tour/welcome1"
  , "tour/welcome4"
  , "tour/basics1"
  , "tour/basics2"
  , "tour/basics3"
  , "tour/basics4"
  , "tour/basics5"
  ]

main : IO ()
main = runner [ tourOfGo ]

