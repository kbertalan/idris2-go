module Data.List.All

import Data.List.Quantifiers

%default total

public export
null :
  {0 ts : List a} ->
  All p ts ->
  Bool
null [] = True
null _ = False

