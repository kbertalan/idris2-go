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

export
keywords : List String
keywords =
  [ "true", "false", "iota", "nil"
  , "int", "int8", "int16", "int32", "int64", "uint"
  , "uint8", "uint16", "uint32", "uint64", "uintptr"
  , "float32", "float64", "complex128", "complex64"
  , "bool", "byte", "rune", "string", "error"
  , "make", "len", "cap", "new", "append", "copy", "close"
  , "delete", "complex", "real", "imag", "panic", "recover"
  , "any", "comparable"
  ]

export
safeGoIdentifier :
  String ->
  String
safeGoIdentifier n =
  if n == replaced then avoidKeyword n
                   else replaced
  where
    replaceChar : Char -> List Char
    replaceChar ' ' = ['_']
    replaceChar  x  = 'u' :: unpack (show $ ord x)

    replaceSpecialChars : (first : Bool) -> List Char -> List Char
    replaceSpecialChars True (x::xs) =
      if isAlpha x || x == '_' then x :: replaceSpecialChars False xs
                               else replaceChar x ++ replaceSpecialChars False xs
    replaceSpecialChars f (x::xs) =
      if isAlpha x || isDigit x || x == '_' then x :: replaceSpecialChars f xs
                                            else replaceChar x ++ replaceSpecialChars f xs
    replaceSpecialChars _ [] = []

    replaced : String
    replaced = pack $ replaceSpecialChars True $ unpack n

    avoidKeyword : String -> String
    avoidKeyword n =
      if n `elem` keywords then "keyword_" ++ n
                           else n

