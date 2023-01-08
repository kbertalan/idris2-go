module Idris2.Compiler.Go

import Core.CompileExpr
import Core.Context
import Core.Directory
import Core.Options

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
  fromGoStmtList ((::) {t} x xs) =
    let MkGoStmts {ts} xs' = fromGoStmtList xs
    in MkGoStmts (x::xs')

namespace GoCaseStmtList

  public export
  data GoCaseStmtList : Type where
    Nil : GoCaseStmtList
    (::) : { t : Type } -> { auto s : Statement t } -> { auto p : Printer t } -> { auto c : IsCaseClause t } -> t -> GoCaseStmtList -> GoCaseStmtList

  public export
  data GoCaseStmts : Type where
    MkGoCaseStmts : { ts : List Type } -> { auto sts : All Statement ts } -> { auto ps : All Printer ts } -> { auto cs : All IsCaseClause ts } -> HList ts -> GoCaseStmts

  export
  fromGoCaseStmtList :
    GoCaseStmtList ->
    GoCaseStmts
  fromGoCaseStmtList [] = MkGoCaseStmts []
  fromGoCaseStmtList ((::) {t} x xs) =
    let MkGoCaseStmts {ts} xs' = fromGoCaseStmtList xs
    in MkGoCaseStmts (x::xs')

record PackageResolver where
  constructor MkPackageResolver
  project : Go.Name -> GoExp
  support : String -> GoExp

goExpArgs : PackageResolver -> List NamedCExp -> GoExpArgs
goExp : PackageResolver -> NamedCExp -> GoExp
goStatement : PackageResolver -> NamedCExp -> GoStmtList
goOp : {0 arity : Nat } -> PackageResolver -> PrimFn arity -> Vect arity NamedCExp -> GoExp
goConCase : PackageResolver -> NamedCExp -> List NamedConAlt -> Maybe NamedCExp -> GoStmtList
goConstCase : PackageResolver -> NamedCExp -> List NamedConstAlt -> Maybe NamedCExp -> GoStmtList

goExpArgs _ [] = MkGoExpArgs []
goExpArgs pr (x::xs) =
  let MkGoExpArgs {ts} {es} {ps} xs' = goExpArgs pr xs
      MkGoExp {t} {e} {p} x' = goExp pr x
  in MkGoExpArgs {ts=t::ts} {es=e::es} {ps=p::ps} $ x' :: xs'

goPrimConst : PackageResolver -> Constant -> GoExp
goPrimConst pr (I i) = MkGoExp $ intL i
goPrimConst pr (I8 i) = MkGoExp $ cast_ int8 $ intL $ cast i
goPrimConst pr (I16 i) = MkGoExp $ cast_ int16 $ intL $ cast i
goPrimConst pr (I32 i) = MkGoExp $ cast_ int32 $ intL $ cast i
goPrimConst pr (I64 i) = MkGoExp $ cast_ int64 $ intL $ cast i
goPrimConst pr (BI i) =
  let MkGoExp fn = pr.support "IntegerLiteral"
  in MkGoExp $ call fn [MkBasicLiteral MkInt $ show i]
goPrimConst pr (B8 m) = MkGoExp $ cast_ uint8 $ intL $ cast m
goPrimConst pr (B16 m) = MkGoExp $ cast_ uint16 $ intL $ cast m
goPrimConst pr (B32 m) = MkGoExp $ cast_ uint32 $ intL $ cast m
goPrimConst pr (B64 m) = MkGoExp $ cast_ uint64 $ intL $ cast m
goPrimConst pr (Str str) = MkGoExp $ stringL str
goPrimConst pr (Ch c) = MkGoExp $ charL c
goPrimConst pr (Db dbl) = MkGoExp $ floatL dbl
goPrimConst pr (PrT pty) = 
  let tyName = case pty of
                 IntType => "IntTypeValue"
                 Int8Type => "Int8TypeValue"
                 Int16Type => "Int16TypeValue"
                 Int32Type => "Int32TypeValue"
                 Int64Type => "Int64TypeValue"
                 IntegerType => "IntegerTypeValue"
                 Bits8Type => "Bits8TypeValue"
                 Bits16Type => "Bits16TypeValue"
                 Bits32Type => "Bits32TypeValue"
                 Bits64Type => "Bits64TypeValue"
                 StringType => "StringTypeValue"
                 CharType => "CharTypeValue"
                 DoubleType => "DoubleTypeValue"
                 WorldType => "WorldTypeValue"
  in pr.support tyName
goPrimConst pr WorldVal = pr.support "World"

goExp _ (NmLocal _ n) = MkGoExp $ id_ $ value $ goName n
goExp pr (NmRef _ n) = pr.project $ goName n
goExp pr (NmLam _ n exp) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [fields [value $ goName n] $ tid' "any"] [fieldT $ tid' "any"] x
goExp pr exp@(NmLet _ n val x) =
  let MkGoStmts stmts = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] stmts) []
goExp pr (NmApp _ fn args) =
  let MkGoExp fn' = goExp pr fn
      MkGoExpArgs args' = goExpArgs pr args
  in MkGoExp $ call fn' args'
goExp pr (NmCon fc n x tag xs) =
  let MkGoExp con = pr.support "Constructor"
      MkGoExpArgs args = goExpArgs pr xs
      tag' = fromMaybe (-1) tag
  in MkGoExp $ call con $ intL tag' :: args
goExp pr exp@(NmOp _ f xs) = goOp pr f xs
goExp pr (NmExtPrim _ p xs) =
  let name = goName p
      MkGoExp fn = pr.support $ name.location.package ++ "_" ++ name.value
      MkGoExpArgs args = goExpArgs pr xs
  in MkGoExp $ call fn args
goExp pr (NmForce fc lz x) =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (paren x') []
goExp pr (NmDelay fc lz x) =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] [ return [ x' ] ]
goExp pr (NmConCase fc sc alts x) =
  let MkGoStmts stmts = fromGoStmtList $ goConCase pr sc alts x
  in MkGoExp $ call (funcL [] [fieldT $ tid' "any"] stmts) []
goExp pr (NmConstCase fc sc alts x) =
  let MkGoStmts stmts = fromGoStmtList $ goConstCase pr sc alts x
  in MkGoExp $ call (funcL [] [fieldT $ tid' "any"] stmts) []
goExp pr (NmPrimVal _ c) = goPrimConst pr c
goExp pr exp@(NmErased fc) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] x
goExp pr exp@(NmCrash fc str) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] x


goOp pr (Add ty) [x,y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerAdd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /+/ y'
goOp pr (Sub ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerSub"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /-/ y'
goOp pr (Mul ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerMul"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /*/ y'
goOp pr (Div ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerDiv"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /// y'
goOp pr (Mod ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerMod"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /%/ y'
goOp pr (Neg ty) [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp ifn = pr.support "IntegerNeg"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x']
        _ => MkGoExp $ minus' x'
goOp pr (ShiftL ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerShiftL"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /<</ y'
goOp pr (ShiftR ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerShiftR"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' />>/ y'
goOp pr (BAnd ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBAnd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /&/ y'
goOp pr (BOr ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /|/ y'
goOp pr (BXOr ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBXOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /^/ y'
goOp pr (LT ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerLT"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /</ y'
goOp pr (LTE ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerLTE"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /<=/ y'
goOp pr (EQ ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerEQ"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' /==/ y'
goOp pr (GTE ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerGTE"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' />=/ y'
goOp pr (GT ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerGT"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ x' />/ y'
goOp pr StrLength [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (id_ "len") [x']
goOp pr StrHead [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ x' `index` intL 0
goOp pr StrTail [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ sliceL x' $ intL 1
goOp pr StrIndex [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
  in MkGoExp $ x' `index` y'
goOp pr StrCons [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp strCons = pr.support "StrCons"
  in MkGoExp $ call strCons [x',y']
goOp pr StrAppend [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
  in MkGoExp $ x' /+/ y'
goOp pr StrReverse [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp strReverse = pr.support "StrReverse"
  in MkGoExp $ call strReverse [x']
goOp pr StrSubstr [x, y, z] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp z' = goExp pr z
  in MkGoExp $ sliceLH x' y' z'
goOp pr DoubleExp [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleExp"
  in MkGoExp $ call fn [x']
goOp pr DoubleLog [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleLog"
  in MkGoExp $ call fn [x']
goOp pr DoublePow [x,y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp fn = pr.support "DoublePow"
  in MkGoExp $ call fn [x', y']
goOp pr DoubleSin [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleSin"
  in MkGoExp $ call fn [x']
goOp pr DoubleCos [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleCos"
  in MkGoExp $ call fn [x']
goOp pr DoubleTan [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleTan"
  in MkGoExp $ call fn [x']
goOp pr DoubleASin [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleASin"
  in MkGoExp $ call fn [x']
goOp pr DoubleACos [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleACos"
  in MkGoExp $ call fn [x']
goOp pr DoubleATan [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleATan"
  in MkGoExp $ call fn [x']
goOp pr DoubleSqrt [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleSqrt"
  in MkGoExp $ call fn [x']
goOp pr DoubleFloor [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleFloor"
  in MkGoExp $ call fn [x']
goOp pr DoubleCeiling [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support "DoubleCeiling"
  in MkGoExp $ call fn [x']
goOp pr (Cast pty1 pty2) [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp fn = pr.support $ "Cast" ++ (category pty1) ++ "To" ++ (type pty2)
  in case castTo pty1 of
      Nothing => MkGoExp $ call fn [x']
      Just ty => MkGoExp $ call fn [cast_ ty x']
  where
    type : PrimType -> String
    type IntType = "Int"
    type Int8Type = "Int8"
    type Int16Type = "Int16"
    type Int32Type = "Int32"
    type Int64Type = "Int64"
    type IntegerType = "Integer"
    type Bits8Type = "UInt8"
    type Bits16Type = "UInt16"
    type Bits32Type = "UInt32"
    type Bits64Type = "UInt64"
    type StringType = "String"
    type CharType = "Char"
    type DoubleType = "Float64"
    type WorldType = "World"

    category : PrimType -> String
    category IntType = "Number"
    category Int8Type = "Number"
    category Int16Type = "Number"
    category Int32Type = "Number"
    category Int64Type = "Number"
    category IntegerType = "Integer"
    category Bits8Type = "Number"
    category Bits16Type = "Number"
    category Bits32Type = "Number"
    category Bits64Type = "Number"
    category StringType = "String"
    category CharType = "Number"
    category DoubleType = "Number"
    category WorldType = "World"

    castTo : PrimType -> Maybe TypeIdentifier
    castTo IntType = Just int
    castTo Int8Type = Just int8
    castTo Int16Type = Just int16
    castTo Int32Type = Just int32
    castTo Int64Type = Just int64
    castTo IntegerType = Nothing
    castTo Bits8Type = Just uint8
    castTo Bits16Type = Just uint16
    castTo Bits32Type = Just uint32
    castTo Bits64Type = Just uint64
    castTo StringType = Just string
    castTo CharType = Just rune
    castTo DoubleType = Just float64
    castTo WorldType = Nothing

goOp pr BelieveMe [_,_,x] = goExp pr x
goOp pr Crash [_,x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] [ expr $ call (id_ "panic") [x'] ]) []
goOp _ _ _ = MkGoExp $ intL (-3)

goConstAlt : PackageResolver -> List NamedConstAlt -> Maybe NamedCExp -> GoCaseStmtList
goConstAlt pr [] (Just def) =
  let MkGoExp defc = goExp pr def
  in [ default_ [ return [ defc ] ] ]
goConstAlt pr [] Nothing =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goConstAlt pr ((MkNConstAlt c exp) :: alts) def =
  let MkGoExp exp' = goExp pr exp
      MkGoExp c' = goPrimConst pr c
  in ( case_ [c'] $ [ return [exp']] ) :: goConstAlt pr alts def

goIntegerConstAlt : PackageResolver -> String -> List NamedConstAlt -> Maybe NamedCExp -> GoCaseStmtList
goIntegerConstAlt pr v [] (Just def) =
  let MkGoExp defc = goExp pr def
  in [ default_ [ return [ defc ] ] ]
goIntegerConstAlt pr v [] Nothing =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goIntegerConstAlt pr v ((MkNConstAlt c exp) :: alts) def =
  let MkGoExp exp' = goExp pr exp
      MkGoExp c' = goPrimConst pr c
  in (case_ [ (call (id_ v /./ "Cmp") [c']) /==/ intL 0 ] [ return [ exp' ] ]) :: goIntegerConstAlt pr v alts def

goConstCase pr exp alts def = cond [(isIntegerConst alts, goIntegerConstCase)] goDefaultConstCase
  where
    goDefaultConstCase : GoStmtList
    goDefaultConstCase =
      let MkGoExp exp' = goExp pr exp
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goConstAlt pr alts def
      in [ switch exp' alts' ]

    isIntegerConst : List NamedConstAlt -> Bool
    isIntegerConst (MkNConstAlt (BI i) _ :: alts) = True
    isIntegerConst _ = False

    goIntegerConstCase : GoStmtList
    goIntegerConstCase =
      let MkGoExp exp' = goExp pr exp
          v = "__switch_var"
          MkGoExp asInteger = pr.support "AsInteger"
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goIntegerConstAlt pr v alts def
      in [ switchS ([id_ v] /:=/ [call asInteger [exp']]) (boolL True) alts' ]

goConAlt : PackageResolver -> String -> List NamedConAlt -> Maybe NamedCExp -> Int -> GoCaseStmtList
goConAlt pr _ [] (Just def) _ =
  let MkGoExp defc = goExp pr def
  in [ default_ [ return [ defc ] ] ]
goConAlt _ _ [] Nothing _ =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goConAlt pr v ((MkNConAlt name _ _ allArgs exp) :: alts) def n =
  let MkGoStmts stmts@(_::_) = fromGoStmtList $ goConAltBody allArgs 0
        | MkGoStmts empty => []
  in (case_ [ intL n ] stmts) :: goConAlt pr v alts def (n+1)
  where
    argList : List Identifier -> GoExpArgs
    argList (x::xs) =
      let MkGoExpArgs xs' = argList xs
      in MkGoExpArgs (x::xs')
    argList [] = MkGoExpArgs []

    goConAltBody : List Core.Name.Name -> Int -> GoStmtList
    goConAltBody [] _ =
      let MkGoStmts stmt = fromGoStmtList $ goStatement pr exp
          allArgs' = map (value . goName) allArgs
          MkGoExpArgs goArgs = argList $ map id_ allArgs'
      in case allArgs' of
          [] => [ return
                  [ call
                    (funcL [] [fieldT $ tid' "any"] stmt)
                    []
                  ]
                ]
          _ => [ return
                 [ call
                    (funcL [fields allArgs' $ tid' "any"] [fieldT $ tid' "any"] stmt)
                    goArgs
                 ]
               ]
    goConAltBody (name :: args) n =
      (decl $ vars [ var [ id_ $ value $ goName name ] (tid' "any") [id_ v /./ "Args" `index` intL n]])
        :: goConAltBody args (n+1)

goConCase pr exp alts def =
  let MkGoExp exp' = goExp pr exp
      v = "__switch_con_var"
      MkGoCaseStmts alts' = fromGoCaseStmtList $ goConAlt pr v alts def 0
      MkGoExp asValue = pr.support "AsValue"
  in [ switchS ([id_ v] /:=/ [call asValue [exp']]) (id_ v /./ "Tag") alts' ]

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
goStatement pr exp@(NmConCase fc sc xs x) = goConCase pr sc xs x
goStatement pr exp@(NmConstCase fc sc xs x) = goConstCase pr sc xs x
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
  goImportExp mod (NmPrimVal fc (BI _)) = addImport (importForSupport mod) empty
  goImportExp mod (NmPrimVal fc cst) = empty
  goImportExp mod (NmErased fc) = empty
  goImportExp mod (NmCrash fc str) = empty

  goImportConAlt mod (MkNConAlt n x tag args y) = addImport (importForSupport mod) $ goImportExp mod y

  goImportConstAlt mod (MkNConstAlt (BI _) x) = addImport (importForSupport mod) $ goImportExp mod x
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
          | False => MkGoExp $ id_ $ capitalize n.value
        i = importForProject moduleName n.location
        True = i /= currentImport
          | False => MkGoExp $ id_ $ capitalize n.value
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
  goImportSpecList currentImport is = sortBy (compare `on` (value . path)) $ go $ SortedMap.toList is
    where
      goPackage : List Import -> List ImportSpec
      goPackage [] = []
      goPackage (x::xs) with (x == currentImport)
        _ | True = goPackage xs
        _ | False =
          let package = packageForImport x is
          in case package == x.package of
                True => import_ x.path :: goPackage xs
                False => importN package x.path :: goPackage xs

      go : List (String, SortedSet Import) -> List ImportSpec
      go [] = []
      go (x::xs) = goPackage (SortedSet.toList $ snd x) ++ go xs

goFile :
  (outDir : String) ->
  (outFile : String) ->
  (moduleName : String) ->
  (List1 (Go.Name, NamedDef)) ->
  Core (Maybe String)
goFile outDir outFile moduleName defs = do
  let (name, _) = head defs
  ensureDirectoryExists (outDir </> name.location.dir)
  let currentImport = importForProject moduleName name.location
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

getGoModule : List String -> Core String
getGoModule directives
    = do let Just mod = getFirstArg directives
              | Nothing => throw (UserError "no go module has been specified, please use --directive module=<go-module-name>")
         pure mod
  where
    getArg : String -> Maybe String
    getArg directive =
      let (k,v) = break (== '=') directive
      in
        if (trim k) == "module"
          then Just $ trim $ substr 1 (length v) v
          else Nothing

    getFirstArg : List String -> Maybe String
    getFirstArg [] = Nothing
    getFirstArg (x::xs) =
      let Just arg = getArg x
            | Nothing => getFirstArg xs
      in Just arg

Go : CG
Go = Other "go"

copySupportFile :
  {auto c : Ref Ctxt Defs} ->
  (outDir : String) ->
  (fname : String) ->
  Core ()
copySupportFile outDir fname = do
  let supportPath = outDir </> "_gen" </> "idris2" </> "support"
  ensureDirectoryExists supportPath
  support <- readDataFile fname
  writeFile (supportPath </> fname) support

export
compileGo :
  {auto c : Ref Ctxt Defs} ->
  (outputDir : String) ->
  (outfile : String) ->
  List (Core.Name.Name, FC, NamedDef) ->
  Core (Maybe String)
compileGo outDir outFile defs = do

  ds <- getDirectives Go
  moduleName <- getGoModule ds

  let supportFiles = ["support.go", "cast.go"]
  for_ supportFiles $ copySupportFile outDir

  let grouppedDefs = getGrouppedDefs defs
  traverse_ (goFile outDir outFile moduleName) grouppedDefs

  pure Nothing

