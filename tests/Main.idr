module Main

import Test.Golden

tourOfGo : TestPool
tourOfGo = MkTestPool "Tour of Go" [] Nothing [ "tour/welcome1" ]

main : IO ()
main = runner [ tourOfGo ]

