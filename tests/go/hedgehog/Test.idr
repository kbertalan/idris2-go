module Test

import Data.List1
import Hedgehog

list1_20 : Gen a -> Gen (List1 a)
list1_20 = list1 (linear 1 20)

prop_const : Property
prop_const = property $ do
  v <- forAll $ list1_20 $ constant 10
  True === all (== 10) v

main : IO ()
main = test . pure $ MkGroup "Test" [ ("constProp", prop_const) ]

