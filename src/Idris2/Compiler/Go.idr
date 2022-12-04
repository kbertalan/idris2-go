module Idris2.Compiler.Go

import Core.CompileExpr
import Core.Context

import Go.AST
import Go.AST.Combinators as Go
import Go.AST.Printer

data GoDecls : Type where

data DeclList : Type where
  Nil : DeclList
  (::) : {t : Type} -> {auto d : Declaration t} -> {auto p : Printer t} -> (a : t) -> DeclList -> DeclList

goUserName :
  UserName ->
  String
goUserName (Basic n) = n
goUserName (Field n) = "field_" ++ n
goUserName Underscore = "_go_underscore"

goName :
  Name ->
  String
goName (NS ns n) = showNSWithSep "_" ns ++ "_" ++ goName n
goName (UN un) = goUserName un
goName (MN mn i) = mn
goName (PV n i) = goName n
goName (DN str n) = str
goName (Nested x n) = goName n
goName (CaseBlock str i) = str
goName (WithBlock str i) = str
goName (Resolved i) = goName (UN Underscore)

goDefs :
  {auto s : Ref GoDecls DeclList} ->
  (Name, FC, NamedDef) ->
  Core ()
goDefs (n, _, nd) = defs nd
  where
    defs : NamedDef -> Core ()
    defs (MkNmFun args exp) = do
      let fnName = goName n
          fnDecl = func fnName [] void []
      decls <- get GoDecls
      put GoDecls (fnDecl :: decls)
      pure ()
    defs (MkNmCon tag arity nt) = pure ()
    defs (MkNmForeign ccs args type) = pure ()
    defs (MkNmError exp) = pure ()

toGoDecls :
  DeclList ->
  (ts ** (All Declaration ts, All Printer ts, HList ts))
toGoDecls [] = ([] ** ([], [], []))
toGoDecls ((::) {t=t} {d=d} {p=p} x xs) =
  let (ts' ** (ds',ps', rest)) = toGoDecls xs
  in (t::ts' ** (d::ds',p::ps',x::rest))

export
compileGo :
  {auto c : Ref Ctxt Defs} ->
  (outfile : String) ->
  List (Name, FC, NamedDef) ->
  Core (Maybe String)
compileGo outfile defs = do
  _ <- newRef GoDecls []

  traverse_ goDefs defs

  goDecls <- get GoDecls
  let (_ ** (_, _, decls)) = toGoDecls goDecls
      src = Go.file outfile (package "main") [] decls

  result <- coreLift $ printFile "." src
  case result of
    Right () => pure Nothing
    Left e => pure $ Just $ show e

