module Idris2.Compiler.Go.Name

import Core.Name
import Core.Name.Namespace

import Data.String

import Libraries.Utils.Path

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
  original : Core.Name.Name

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
  , "break", "default", "func", "interface", "select"
  , "case", "defer", "go", "map", "struct"
  , "chan", "else", "goto", "package", "switch"
  , "const", "fallthrough", "if", "range", "type"
  , "continue", "for", "import", "return", "var"
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
    replaceChar ',' = ['_']
    replaceChar '.' = ['_']
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

export
goSupportNamespace : Namespace
goSupportNamespace = unsafeFoldNamespace ["support", "idris2", "_gen"]

export
goSupportLocation : Location
goSupportLocation = (MkLocation ("_gen" </> "idris2" </> "support") "support.go" "support")

export
goTailRecName : Core.Name.Name
goTailRecName = NS goSupportNamespace (UN $ Basic "unused_should_panic")

export
goLocationFromNS :
  Namespace ->
  Location
goLocationFromNS ns =
  let parts = map toLower $ reverse $ unsafeUnfoldNamespace ns
      name = concat $ intersperse "_" parts
      name' = if name == "test" then "test_" else name
  in MkLocation "" ("module_" ++ name' ++ ".go") "main"

export
goNameFromNS :
  Namespace ->
  String
goNameFromNS ns =
  let parts = map toLower $ reverse $ unsafeUnfoldNamespace ns
  in concat $ intersperse "_" parts

export
goUserName :
  UserName ->
  String
goUserName (Basic n) = safeGoIdentifier n
goUserName (Field n) = safeGoIdentifier $ n ++ "__field"
goUserName Underscore = "Underscore__"

export
goName :
  Core.Name.Name ->
  Go.Name.Name
goName orig@(NS ns n) = let sub = goName n in MkName (goLocationFromNS ns) (safeGoIdentifier $ goNameFromNS ns ++ "_" ++ sub.value) orig
goName orig@(UN un) = MkName (MkLocation "" "user_generated_.go" "main") (goUserName un) orig
goName orig@(MN mn i) = MkName (MkLocation "" "machine_generated_.go" "main") (safeGoIdentifier $ mn ++ show i) orig
goName orig@(PV n i) = let sub = goName n in MkName sub.location (sub.value ++ "_" ++ show i) orig
goName orig@(DN str n) = let sub = goName n in MkName sub.location (safeGoIdentifier $ sub.value ++ "_" ++ str) orig
goName orig@(Nested (x,y) n) = let sub = goName n in MkName sub.location (sub.value ++ "_" ++ show x ++ "_" ++ show y) orig
goName orig@(CaseBlock str i) = MkName empty (safeGoIdentifier $ str ++ show i) orig
goName orig@(WithBlock str i) = MkName empty (safeGoIdentifier $ str ++ show i) orig
goName orig@(Resolved i) = MkName empty ("resolved" ++ show i) orig

