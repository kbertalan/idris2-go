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

record PackageResolver where
  constructor MkPackageResolver
  project : Go.Name -> GoExp
  support : String -> GoExp

goExpArgs : PackageResolver -> List NamedCExp -> GoExpArgs
goExp : PackageResolver -> NamedCExp -> GoExp
goStatement : PackageResolver -> NamedCExp -> GoStmtList

goExpArgs _ [] = MkGoExpArgs []
goExpArgs pr (x::xs) =
  let MkGoExpArgs {ts} {es} {ps} xs' = goExpArgs pr xs
      MkGoExp {t} {e} {p} x' = goExp pr x
  in MkGoExpArgs {ts=t::ts} {es=e::es} {ps=p::ps} $ x' :: xs'

goExp _ (NmLocal _ n) = MkGoExp $ id_ $ value $ goName n
goExp pr (NmRef _ n) = pr.project $ goName n
goExp pr (NmLam _ n exp) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [fields [value $ goName n] $ tid' "any"] [fieldT $ tid' "any"] x
goExp pr (NmLet _ n val x) =
  let MkGoExp val' = goExp pr val
      MkGoStmts x' = fromGoStmtList $ goStatement pr x
  in MkGoExp $ call (paren $ funcL [] [] x') []
goExp pr (NmApp _ fn args) =
  let MkGoExp fn' = goExp pr fn
      MkGoExpArgs args' = goExpArgs pr args
  in MkGoExp $ call fn' args'
goExp pr (NmCon fc n x tag xs) =
  let MkGoExp con = pr.support "Constructor"
      MkGoExpArgs args = goExpArgs pr xs
      tag' = fromMaybe (-1) tag
  in MkGoExp $ call con $ intL tag' :: args
-- goExp pr (NmOp fc f xs) = ?aaa_6
-- goExp pr (NmExtPrim fc p xs) = ?aaa_7
goExp pr (NmForce fc lz x) =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (paren x') []
goExp pr (NmDelay fc lz x) =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] [ return [ x' ] ]
-- goExp pr (NmConCase fc sc xs x) = ?aaa_10
-- goExp pr (NmConstCase fc sc xs x) = ?aaa_11
goExp pr (NmPrimVal fc (I i)) = MkGoExp $ intL i
goExp pr (NmPrimVal fc (I8 i)) = MkGoExp $ cast_ int8 $ intL $ cast i
goExp pr (NmPrimVal fc (I16 i)) = MkGoExp $ cast_ int16 $ intL $ cast i
goExp pr (NmPrimVal fc (I32 i)) = MkGoExp $ cast_ int32 $ intL $ cast i
goExp pr (NmPrimVal fc (I64 i)) = MkGoExp $ cast_ int64 $ intL $ cast i
goExp pr (NmPrimVal fc (BI i)) = MkGoExp $ intL $ cast i
goExp pr (NmPrimVal fc (B8 m)) = MkGoExp $ cast_ uint8 $ intL $ cast m
goExp pr (NmPrimVal fc (B16 m)) = MkGoExp $ cast_ uint16 $ intL $ cast m
goExp pr (NmPrimVal fc (B32 m)) = MkGoExp $ cast_ uint32 $ intL $ cast m
goExp pr (NmPrimVal fc (B64 m)) = MkGoExp $ cast_ uint64 $ intL $ cast m
goExp pr (NmPrimVal fc (Str str)) = MkGoExp $ stringL str
goExp pr (NmPrimVal fc (Ch c)) = MkGoExp $ stringL $ pack [c] -- TODO replace with char literal
goExp pr (NmPrimVal fc (Db dbl)) = MkGoExp $ floatL dbl
goExp pr (NmPrimVal fc (PrT pty)) = MkGoExp $ stringL "TYPE" -- TODO
goExp pr (NmPrimVal fc WorldVal) = MkGoExp $ stringL "WORLD"
goExp pr exp@(NmErased fc) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] x
goExp pr exp@(NmCrash fc str) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] x
goExp _ _ = MkGoExp $ intL (-1)

goStatement pr exp@(NmLocal _ _) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmRef _ _) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmLam _ _ _) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmLet _ n val x) =
  let MkGoExp val' = goExp pr val 
  in decl (vars [ var [id_ $ value $ goName n] (tid' "any") [val'] ]) :: goStatement pr x
goStatement pr exp@(NmApp fc x xs) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmCon fc n x tag xs) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmOp fc f xs) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmExtPrim fc p xs) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmForce fc lz x) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmDelay fc lz x) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmConCase fc sc xs x) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmConstCase fc sc xs x) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmPrimVal fc cst) = let MkGoExp x = goExp pr exp in [ return [x] ]
goStatement pr exp@(NmErased _) = [ expr $ call (id_ "panic") [stringL "executing erased term"] ]
goStatement pr exp@(NmCrash _ str) = [ expr $ call (id_ "panic") [stringL str] ]

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
  PackageResolver ->
  (Go.Name,NamedDef) ->
  Core ()
goDefs pr (n, nd) = defs nd
  where
    defs : NamedDef -> Core ()
    defs (MkNmFun args exp) = do
      let MkGoStmts sts = fromGoStmtList $ goStatement pr exp
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

  export
  goSupport :
    (moduleName : String) ->
    Imports ->
    String ->
    GoExp
  goSupport moduleName is n =
    let package = packageForImport (importForSupport moduleName) is
    in MkGoExp $ id_ package /./ capitalize n

  export
  goImportSpecList :
    (currentImport : Import) ->
    Imports ->
    List ImportSpec
  goImportSpecList currentImport is = go $ SortedMap.toList is
    where
      goPackage : List Import -> List ImportSpec
      goPackage [] = []
      goPackage (x::xs) = if x == currentImport
                            then goPackage xs
                            else importN (packageForImport x is) x.path :: goPackage xs

      go : List (String, SortedSet Import) -> List ImportSpec
      go [] = []
      go (x::xs) = goPackage (SortedSet.toList $ snd x) ++ go xs

goFile :
  (outDir : String) ->
  (outFile : String) ->
  (List1 (Go.Name, NamedDef)) ->
  Core (Maybe String)
goFile outDir outFile defs = do
  let (name, _) = head defs
  ensureDirectoryExists (outDir </> name.location.dir)
  let moduleName = "github.com/kbertalan/idris2-go" -- TODO detect module name or get as a compiler parameter
      currentImport = importForProject moduleName name.location
      imports = goImportDefs moduleName $ forget defs
      packageResolver = MkPackageResolver
                          { project = goRef moduleName currentImport imports
                          , support = goSupport moduleName imports
                          }

  _ <- newRef Decls []
  traverse_ (goDefs packageResolver) $ forget defs

  goDecls <- get Decls
  let MkGoDecls decls = fromGoDecls goDecls
      src = Go.file (name.location.dir </> name.location.fileName) (package name.location.package) (goImportSpecList currentImport imports) decls

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

