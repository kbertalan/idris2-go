module Main

import Test.Golden

tourOfGo : TestPool
tourOfGo = MkTestPool "Tour of Go" [] Nothing
  [ "tour/welcome1"
  , "tour/welcome4"
  ]

main : IO ()
main = runner [ tourOfGo ]

