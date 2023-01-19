module Data.List.All

import Data.List
import Data.List.Quantifiers

%default total

public export
head : All p (t::ts) -> p t
head (x::_) = x

public export
null :
  {0 ts : List a} ->
  All p ts ->
  Bool
null [] = True
null _ = False

public export
length :
  {0 ts : List a} ->
  All p ts ->
  Nat
length [] = 0
length (x::xs) = 1 + length xs

