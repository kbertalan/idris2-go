module Data.List.All

import Data.List
import Data.List.Last
import Data.List.Quantifiers

public export
head : All p (t::ts) -> p t
head (x::_) = x

public export
head' : {0 ts : List a }
  -> All p ts
  -> Maybe $ case ts of
              [] => Void
              x::_ => p x
head' [] = Nothing
head' xs@(_::_) = Just $ head xs

public export
last : { auto 0 _ : NonEmpty ts } -> { auto lst : Last t ts } -> All p ts -> p t
last {lst = ItIsLast} [x] = x
last {lst = Later l} (y :: xs@(_::_)) = last {lst = l} xs

public export
last' : {0 ts : List a}
  -> {_ : Last t ts}
  -> All p ts
  -> Maybe $ case ts of
              [] => Void
              xs => p t
last' [] = Nothing
last' xs@(_::_) = Just $ last xs

