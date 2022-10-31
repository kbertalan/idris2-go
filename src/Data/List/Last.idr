module Data.List.Last

import Control.Function
import Decidable.Equality

public export
data Last : a -> List a -> Type where
  ItIsLast : Last x [x]
  Later : Last y xs -> Last y (x :: xs)

public export
implementation Uninhabited (ItIsLast {x} = Later l {x}) where
  uninhabited Refl impossible

public export
implementation Uninhabited (Later l {x} = ItIsLast {x}) where
  uninhabited Refl impossible

public export
implementation Injective (Later {y} {x} {xs}) where
  injective Refl = Refl

public export
implementation Uninhabited (Last x Nil) where
  uninhabited ItIsLast impossible
  uninhabited (Later _) impossible

public export
implementation Uninhabited (x = y) => Uninhabited (Last y xs) => Uninhabited (Last y (x::xs)) where
  uninhabited ItIsLast @{a} = uninhabited Refl @{a}
  uninhabited (Later l) = uninhabited l

public export
implementation DecEq (Last x xs) where
  decEq ItIsLast ItIsLast = Yes Refl
  decEq (Later this) (Later that) = decEqCong $ decEq this that
  decEq ItIsLast (Later that) = No absurd
  decEq (Later this) ItIsLast = No absurd

public export
isLast : DecEq a => (x : a) -> (xs : List a) -> Dec (Last x xs)
isLast x [] = No absurd
isLast x [y] with (decEq x y)
  isLast x [x] | Yes Refl = Yes ItIsLast
  _ | No xny = No $ \case
    ItIsLast => xny Refl
    Later _ impossible
isLast x (y :: xs@(_::_)) with (isLast x xs)
  _ | Yes prf = Yes $ Later prf
  _ | No xnlxs = No $ \case
    ItIsLast impossible
    Later l => xnlxs l