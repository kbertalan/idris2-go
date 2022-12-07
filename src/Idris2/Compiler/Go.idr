module Idris2.Compiler.Go

import Core.CompileExpr
import Core.Context
import Core.Directory

import Data.List
import Data.List1
import Data.SortedMap
import Data.String

import Go.AST
import Go.AST.Combinators as Go
import Go.AST.Printer

import Idris2.Compiler.Go.Name as Go

import Libraries.Utils.Path

goLocationFromNS :
  Namespace ->
  Location
goLocationFromNS ns =
  let parts = map toLower $ reverse $ unsafeUnfoldNamespace ns
      package = case parts of
                  _::_ => last parts
                  _ => ""

  in MkLocation (joinPath parts) (package ++ ".go") package

goUserName :
  UserName ->
  String
goUserName (Basic n) = safeGoIdentifier n
goUserName (Field n) = safeGoIdentifier $ n ++ "__field"
goUserName Underscore = "Underscore__"

goName :
  Core.Name.Name ->
  Go.Name
goName (NS ns n) = let sub = goName n in MkName (goLocationFromNS ns) sub.value
goName (UN un) = MkName (MkLocation "gen/go" "user.go" "go") $ goUserName un
goName (MN mn i) = MkName (MkLocation "gen/go" "generated.go" "go") (safeGoIdentifier $ mn ++ show i)
goName (PV n i) = let sub = goName n in MkName sub.location (sub.value ++ show i)
goName (DN str n) = goName n
goName (Nested x n) = goName n
goName (CaseBlock str i) = MkName empty $ safeGoIdentifier $ str ++ show i
goName (WithBlock str i) = MkName empty $ safeGoIdentifier $ str ++ show i
goName (Resolved i) = MkName empty ("resolved" ++ show i)

data GoDecls : Type where

data DeclList : Type where
  Nil : DeclList
  (::) : {t : Type} -> {auto d : Declaration t} -> {auto p : Printer t} -> (a : t) -> DeclList -> DeclList

goDefs :
  {auto s : Ref GoDecls DeclList} ->
  (Go.Name,NamedDef) ->
  Core ()
goDefs (n, nd) = defs nd
  where
    defs : NamedDef -> Core ()
    defs (MkNmFun args exp) = do
      let fnDecl = func (capitalize n.value) [fields (map (value . goName) args) $ tid' "any" ] void
                    [ [ id_ "i" ] /:=/ [ intL 0 ] |> doc (show exp)
                    ]
      decls <- get GoDecls
      put GoDecls (fnDecl :: decls)
      pure ()
    defs (MkNmCon tag arity nt) = pure ()
    defs (MkNmForeign ccs args type) = do
      let fnDecl = func (capitalize n.value) [] void []
      decls <- get GoDecls
      put GoDecls (fnDecl :: decls)
      pure ()
    defs (MkNmError exp) = assert_total $ idris_crash ("Error with expression: " ++ show exp)

toGoDecls :
  DeclList ->
  (ts ** (All Declaration ts, All Printer ts, HList ts))
toGoDecls [] = ([] ** ([], [], []))
toGoDecls ((::) {t=t} {d=d} {p=p} x xs) =
  let (ts' ** (ds',ps', rest)) = toGoDecls xs
  in (t::ts' ** (d::ds',p::ps',x::rest))

goFile :
  (outDir : String) ->
  (outFile : String) ->
  (List1 (Go.Name, NamedDef)) ->
  Core (Maybe String)
goFile outDir outFile defs = do
  _ <- newRef GoDecls []
  let (name, _) = head defs
  ensureDirectoryExists (outDir </> name.location.dir)

  traverse_ goDefs $ forget defs

  goDecls <- get GoDecls
  let (_ ** (_, _, decls)) = toGoDecls goDecls
      src = Go.file (name.location.dir </> name.location.fileName) (package name.location.package) [] decls

  result <- coreLift $ printFile outDir src
  case result of
    Right () => pure Nothing
    Left e => pure $ Just $ show e

getGrouppedDefs :
  List (Core.Name.Name, FC, NamedDef) ->
  List (List1 (Go.Name, NamedDef))
getGrouppedDefs defs =
  groupBy ((==) `on` locationOf)
    $ sortBy (compare `on` locationOf)
    $ map (\(n, _, d) => (goName n,d)) defs
  where
    locationOf : (Go.Name, _) -> Location
    locationOf = location . fst

export
compileGo :
  {auto c : Ref Ctxt Defs} ->
  (outputDir : String) ->
  (outfile : String) ->
  List (Core.Name.Name, FC, NamedDef) ->
  Core (Maybe String)
compileGo outDir outFile defs = do

  let grouppedDefs = getGrouppedDefs defs
  traverse_ (goFile outDir outFile) grouppedDefs

  pure Nothing

