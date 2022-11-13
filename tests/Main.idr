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
  ]

main : IO ()
main = runner [ tourOfGo ]

