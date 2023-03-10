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
  , "tour/basics6"
  , "tour/basics7"
  , "tour/basics8"
  , "tour/basics9"
  , "tour/basics10"
  , "tour/basics11"
  , "tour/basics12"
  , "tour/basics13"
  , "tour/basics14"
  , "tour/basics15"
  , "tour/basics16"
  , "tour/flowcontrol1"
  , "tour/flowcontrol3"
  , "tour/flowcontrol4"
  , "tour/flowcontrol5"
  , "tour/flowcontrol6"
  , "tour/flowcontrol7"
  , "tour/flowcontrol9"
  , "tour/flowcontrol10"
  , "tour/flowcontrol11"
  , "tour/flowcontrol12"
  , "tour/flowcontrol13"
  , "tour/moretypes1"
  , "tour/moretypes2"
  , "tour/moretypes3"
  , "tour/moretypes4"
  , "tour/moretypes5"
  , "tour/moretypes6"
  , "tour/moretypes7"
  , "tour/moretypes8"
  , "tour/moretypes9"
  , "tour/moretypes10"
  , "tour/moretypes11"
  , "tour/moretypes12"
  , "tour/moretypes13"
  , "tour/moretypes14"
  , "tour/moretypes15"
  , "tour/moretypes16"
  , "tour/moretypes17"
  , "tour/moretypes19"
  , "tour/moretypes20"
  , "tour/moretypes21"
  , "tour/moretypes22"
  , "tour/moretypes24"
  , "tour/moretypes25"
  ]

goTest : TestPool
goTest = MkTestPool "Compile to Go" [] Nothing
  [ "go/simple"
  , "go/ffi"
  , "go/benchmark"
  ]

main : IO ()
main = runner [ tourOfGo, goTest ]

