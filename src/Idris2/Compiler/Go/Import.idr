module Idris2.Compiler.Go.Import

import Data.Fin
import Data.List
import public Data.SortedMap
import public Data.SortedSet

import Idris2.Compiler.Go.Name

public export
Module : Type
Module = String

public export
data ImportType
  = Project
  | Support
  | External

export
implementation Eq ImportType where
  Project == Project = True
  Support == Support = True
  External == External = True
  _ == _ = False

export
implementation Ord ImportType where
  compare Project Project = EQ
  compare Project _ = LT
  compare Support Project = GT
  compare Support Support = EQ
  compare Support External = LT
  compare External External = EQ
  compare External _ = GT

public export
record Import where
  constructor MkImport
  type : ImportType
  path : String
  package : String

export
implementation Eq Import where
  a == b = a.type == b.type && a.path == b.path && a.package == b.package

export
implementation Ord Import where
  compare a b = compare a.type b.type
                  <+> compare a.path b.path
                  <+> compare a.package b.package

public export
Imports : Type
Imports = SortedMap String (SortedSet Import)

export
addImport :
  Import ->
  Imports ->
  Imports
addImport i is = merge is $ singleton i.package (singleton i)

export
importForSupport :
  Module ->
  Import
importForSupport mod = MkImport Support (mod ++ "/_gen/idris2/support") "support"

export
importForProject :
  Module ->
  Location ->
  Import
importForProject mod loc =
  if loc == goSupportLocation then importForSupport mod
                              else MkImport Project mod loc.package

export
importForMain :
  Module ->
  Import
importForMain mod = MkImport Project mod "main"

export
importForExternal :
  (path : String) ->
  (package : String) ->
  Import
importForExternal = MkImport External

export
packageForImport :
  Import ->
  Imports ->
  String
packageForImport i is =
  let Just cis = lookup i.package is
        | Nothing => i.package
      Just index : Maybe Integer = map cast $ findIndex ((==) i.path . path) $ SortedSet.toList cis
        | Nothing => i.package
  in case index of
      0 => i.package
      _ => i.package ++ "_" ++ show index

