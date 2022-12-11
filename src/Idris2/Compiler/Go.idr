module Idris2.Compiler.Go

import Core.CompileExpr
import Core.Context
import Core.Directory

import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.Vect

import Go.AST
import Go.AST.Combinators as Go
import Go.AST.Printer

import Idris2.Compiler.Go.Import
import Idris2.Compiler.Go.Name as Go

import Libraries.Utils.Path

namespace GoExp

  public export
  data GoExp : Type where
    MkGoExp : { t : Type } -> { auto e : Expression t } -> { auto p : Printer t } -> t -> GoExp

namespace GoExpArgs

  public export
  data GoExpArgs : Type where
    MkGoExpArgs : { ts : List Type } -> { auto es : All Expression ts } -> { auto ps : All Printer ts } -> HList ts -> GoExpArgs

namespace GoStmtList

  public export
  data GoStmtList : Type where
    Nil : GoStmtList
    (::) : { t : Type } -> { auto s : Statement t } -> { auto p : Printer t } -> t -> GoStmtList -> GoStmtList

  public export
  data GoStmts : Type where
    MkGoStmts : { ts : List Type } -> { auto sts : All Statement ts } -> { auto ps : All Printer ts } -> HList ts -> GoStmts

  export
  fromGoStmtList :
    GoStmtList ->
    GoStmts
  fromGoStmtList [] = MkGoStmts []
  fromGoStmtList ((::) {t} {s} {p} x xs) =
    let MkGoStmts {ts} {sts} {ps} xs' = fromGoStmtList xs
    in MkGoStmts (x::xs')

RefResolver : Type
RefResolver = Go.Name -> GoExp

goExpArgs : RefResolver -> List NamedCExp -> GoExpArgs
goExp : RefResolver -> NamedCExp -> GoExp
goStatement : RefResolver -> NamedCExp -> GoStmtList

goExpArgs _ [] = MkGoExpArgs []
goExpArgs rr (x::xs) =
  let MkGoExpArgs {ts} {es} {ps} xs' = goExpArgs rr xs
      MkGoExp {t} {e} {p} x' = goExp rr x
  in MkGoExpArgs {ts=t::ts} {es=e::es} {ps=p::ps} $ x' :: xs'

goExp _ (NmLocal _ n) = MkGoExp $ id_ $ value $ goName n
goExp rr (NmRef _ n) = rr $ goName n
goExp rr (NmLam _ n exp) =
  let MkGoStmts x = fromGoStmtList $ goStatement rr exp
  in MkGoExp $ funcL [fields [value $ goName n] $ tid' "any"] [fieldT $ tid' "any"] x
goExp rr (NmLet _ n val x) =
  let MkGoExp val' = goExp rr val
      MkGoStmts x' = fromGoStmtList $ goStatement rr x
  in MkGoExp $ call (paren $ funcL [] [] x') []
goExp rr (NmApp _ fn args) =
  let MkGoExp fn' = goExp rr fn
      MkGoExpArgs args' = goExpArgs rr args
  in MkGoExp $ call fn' args'
-- goExp rr (NmCon fc n x tag xs) = ?aaa_5
-- goExp rr (NmOp fc f xs) = ?aaa_6
-- goExp rr (NmExtPrim fc p xs) = ?aaa_7
-- goExp rr (NmForce fc lz x) = ?aaa_8
-- goExp rr (NmDelay fc lz x) = ?aaa_9
-- goExp rr (NmConCase fc sc xs x) = ?aaa_10
-- goExp rr (NmConstCase fc sc xs x) = ?aaa_11
-- goExp rr (NmPrimVal fc cst) = ?aaa_12
-- goExp rr (NmErased fc) = ?aaa_13
-- goExp rr (NmCrash fc str) = ?aaa_14
goExp _ _ = MkGoExp $ intL (-1)

goStatement rr exp@(NmLocal _ _) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmRef _ _) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmLam _ _ _) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmLet _ n val x) =
  let MkGoExp val' = goExp rr val 
  in decl (vars [ var [id_ $ value $ goName n] (tid' "any") [val'] ]) :: goStatement rr x
goStatement rr exp@(NmApp fc x xs) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmCon fc n x tag xs) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmOp fc f xs) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmExtPrim fc p xs) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmForce fc lz x) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmDelay fc lz x) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmConCase fc sc xs x) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmConstCase fc sc xs x) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmPrimVal fc cst) = let MkGoExp x = goExp rr exp in [ return [x] ]
goStatement rr exp@(NmErased _) = [ expr $ call (id_ "panic") [stringL "executing erased term"] ]
goStatement rr exp@(NmCrash _ str) = [ expr $ call (id_ "panic") [stringL str] ]

data Decls : Type where

namespace GoDecls

  public export
  data GoDeclList : Type where
    Nil : GoDeclList
    (::) : {t : Type} -> {auto d : Declaration t} -> {auto p : Printer t} -> (a : t) -> GoDeclList -> GoDeclList

  public export
  data GoDecls : Type where
    MkGoDecls : { ts : List Type } -> {auto ds : All Declaration ts} -> {auto ps : All Printer ts} -> HList ts -> GoDecls

  export
  fromGoDecls :
    GoDeclList ->
    GoDecls
  fromGoDecls [] = MkGoDecls []
  fromGoDecls ((::) {t} {d} {p} x xs) =
    let MkGoDecls {ts} {ds} {ps} xs' = fromGoDecls xs
    in MkGoDecls {ts=t::ts} {ds=d::ds} {ps=p::ps} $ x :: xs'

goDefs :
  {auto s : Ref Decls GoDeclList} ->
  RefResolver ->
  (Go.Name,NamedDef) ->
  Core ()
goDefs rr (n, nd) = defs nd
  where
    defs : NamedDef -> Core ()
    defs (MkNmFun args exp) = do
      let MkGoStmts sts = fromGoStmtList $ goStatement rr exp
          fnDecl = docs [show exp, show n.original] $
                    func (capitalize n.value) [fields (map (value . goName) args) $ tid' "any" ] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (MkNmCon tag arity nt) = pure ()
    defs (MkNmForeign ccs args type) = do
      let fnDecl = func (capitalize n.value) [] void []
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (MkNmError exp) = assert_total $ idris_crash ("Error with expression: " ++ show exp)

namespace GoImports

  export
  goImportDefs :
    (moduleName : String) ->
    List (Go.Name, NamedDef) ->
    Imports

  goImportDef :
    (moduleName : String) ->
    (Go.Name, NamedDef) ->
    Imports

  goImportExp :
    (moduleName : String)->
    NamedCExp ->
    Imports

  goImportConAlt :
    (moduleName : String) ->
    NamedConAlt ->
    Imports

  goImportConstAlt :
    (moduleName : String) ->
    NamedConstAlt ->
    Imports

  goImportDefs _ [] = empty
  goImportDefs mod (x::xs) =
    merge (goImportDef mod x) $ goImportDefs mod xs

  goImportDef mod (n, (MkNmFun args x)) = goImportExp mod x
  goImportDef mod (n, (MkNmCon tag arity nt)) = empty
  goImportDef mod (n, (MkNmForeign ccs fargs x)) = empty -- TODO parse imports from FFI
  goImportDef mod (n, (MkNmError x)) = goImportExp mod x

  goImportExp mod (NmLocal fc n) = empty
  goImportExp mod (NmRef fc n) = let name = goName n in addImport (importForProject mod name.location) empty
  goImportExp mod (NmLam fc x y) = goImportExp mod y
  goImportExp mod (NmLet fc x y z) = merge (goImportExp mod y) $ goImportExp mod z
  goImportExp mod (NmApp fc x xs) = foldl (\acc => merge acc . goImportExp mod) (goImportExp mod x) xs
  goImportExp mod (NmCon fc n x tag xs) = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
  goImportExp mod (NmOp fc f xs) = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
  goImportExp mod (NmExtPrim fc p xs) = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
  goImportExp mod (NmForce fc lz x) = addImport (importForSupport mod) $ goImportExp mod x
  goImportExp mod (NmDelay fc lz x) = addImport (importForSupport mod) $ goImportExp mod x
  goImportExp mod (NmConCase fc sc xs x) =
    let ix = maybe empty (goImportExp mod) x
        isc = merge ix $ goImportExp mod sc
    in foldl (\acc => merge acc . goImportConAlt mod) isc xs
  goImportExp mod (NmConstCase fc sc xs x) =
    let ix = maybe empty (goImportExp mod) x
        isc = merge ix $ goImportExp mod sc
    in foldl (\acc => merge acc . goImportConstAlt mod) isc xs
  goImportExp mod (NmPrimVal fc cst) = empty
  goImportExp mod (NmErased fc) = empty
  goImportExp mod (NmCrash fc str) = empty

  goImportConAlt mod (MkNConAlt n x tag args y) = goImportExp mod y

  goImportConstAlt mod (MkNConstAlt cst x) = goImportExp mod x

  export
  goRef :
    (moduleName : String) ->
    (currentImport : Import) ->
    Imports ->
    Go.Name ->
    GoExp
  goRef moduleName currentImport is n =
    let True = n.location /= empty
          | False => MkGoExp $ id_ n.value
        i = importForProject moduleName n.location
        True = i /= currentImport
          | False => MkGoExp $ id_ n.value
        package = packageForImport i is
    in MkGoExp $ id_ package /./ capitalize n.value

goFile :
  (outDir : String) ->
  (outFile : String) ->
  (List1 (Go.Name, NamedDef)) ->
  Core (Maybe String)
goFile outDir outFile defs = do
  let (name, _) = head defs
  ensureDirectoryExists (outDir </> name.location.dir)
  let moduleName = "github.com/kbertalan/idris2-go"
      currentImport = importForProject moduleName name.location
      imports = goImportDefs moduleName $ forget defs
      refResolver = goRef moduleName currentImport imports

  _ <- newRef Decls []
  traverse_ (goDefs refResolver) $ forget defs

  goDecls <- get Decls
  let MkGoDecls decls = fromGoDecls goDecls
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

