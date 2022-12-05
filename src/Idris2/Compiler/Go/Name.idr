module Idris2.Compiler.Go.Name

public export
record Location where
  constructor MkLocation
  dir : String
  fileName : String
  package : String

export
empty : Location
empty = MkLocation "" "" ""

export
implementation Eq Location where
  (==) a b = a.dir == b.dir && a.fileName == b.fileName && a.package == b.package

export
implementation Ord Location where
  compare a b = compare a.dir b.dir
                  <+> compare a.fileName b.fileName
                  <+> compare a.package b.package

public export
record Name where
  constructor MkName
  location : Location
  value : String

export
capitalize : String -> String
capitalize str =
  case unpack str of
    x :: xs => pack $ (toUpper x) :: xs
    _ => ""
