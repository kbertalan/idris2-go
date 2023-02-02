module Idris2.Compiler.Go

import Compiler.ES.TailRec

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

import Idris2.Compiler.Go.GoC
import Idris2.Compiler.Go.Import
import Idris2.Compiler.Go.Name as Go
import Idris2.Compiler.Go.Support.Gen

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

namespace Support

  export
  supportCast :
    Expression e =>
    GoType t =>
    e ->
    t ->
    TypeAssertExpression (CastExpression TypeIdentifier e) t
  supportCast e t = typeAssert (cast_ (tid' "any") e) t

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
goPrimConst pr (I8 i) = MkGoExp $ cast_ int8 $ MkBasicLiteral MkInt $ show i
goPrimConst pr (I16 i) = MkGoExp $ cast_ int16 $ MkBasicLiteral MkInt $ show i
goPrimConst pr (I32 i) = MkGoExp $ cast_ int32 $ MkBasicLiteral MkInt $ show i
goPrimConst pr (I64 i) = MkGoExp $ cast_ int64 $ MkBasicLiteral MkInt $ show i
goPrimConst pr (BI i) =
  let MkGoExp fn = pr.support "IntegerLiteral"
  in MkGoExp $ call fn [stringL $ show i]
goPrimConst pr (B8 m) = MkGoExp $ cast_ uint8 $ MkBasicLiteral MkInt $ show m
goPrimConst pr (B16 m) = MkGoExp $ cast_ uint16 $ MkBasicLiteral MkInt $ show m
goPrimConst pr (B32 m) = MkGoExp $ cast_ uint32 $ MkBasicLiteral MkInt $ show m
goPrimConst pr (B64 m) = MkGoExp $ cast_ uint64 $ MkBasicLiteral MkInt $ show m
goPrimConst pr (Str str) = MkGoExp $ stringL str
goPrimConst pr (Ch c) = MkGoExp $ cast_ uint8 $ charL c
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
goPrimConst pr WorldVal =
  let MkGoExp newWorld = pr.support "NewWorld"
  in MkGoExp $ call newWorld []

goExp _ (NmLocal _ n) = MkGoExp $ id_ $ value $ goName n
goExp pr (NmRef _ n) = pr.project $ goName n
goExp pr (NmLam _ n exp) =
  let MkGoStmts x = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ funcL [fields [value $ goName n] $ tid' "any"] [fieldT $ tid' "any"] x
goExp pr exp@(NmLet _ n val x) =
  let MkGoStmts stmts = fromGoStmtList $ goStatement pr exp
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] stmts) []
goExp pr (NmApp _ fn@(NmRef _ _) args) =
  let MkGoExp fn' = goExp pr fn
      MkGoExpArgs args' = goExpArgs pr args
  in MkGoExp $ call fn' args'
goExp pr (NmApp _ fn []) =
  let MkGoExp fn' = goExp pr fn
  in MkGoExp $ call (typeAssert fn' (func' [] [fieldT $ tid' "any"])) []
goExp pr (NmApp _ fn args) =
  let MkGoExp fn' = goExp pr fn
      MkGoExpArgs args' = goExpArgs pr args
  in MkGoExp $ call (typeAssert fn' (func' [fields ["a" ++ show i | i <- [1..length args]] $ tid' "any"] [fieldT $ tid' "any"])) args'
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
  in MkGoExp $ call (typeAssert (paren x') (func' [] [fieldT $ tid' "any"])) []
goExp pr (NmDelay fc lz x) =
  let MkGoExp x' = goExp pr x
      MkGoExp delay = pr.support "Delay"
  in MkGoExp $ call delay [funcL [] [fieldT $ tid' "any"] [ return [ x' ] ]]
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


goPrimType : PrimType -> TypeIdentifier
goPrimType IntType = int
goPrimType Int8Type = int8
goPrimType Int16Type = int16
goPrimType Int32Type = int32
goPrimType Int64Type = int64
goPrimType IntegerType = tid' "support.IntegerType"
goPrimType Bits8Type = uint8
goPrimType Bits16Type = uint16
goPrimType Bits32Type = uint32
goPrimType Bits64Type = uint64
goPrimType StringType = string
goPrimType CharType = uint8
goPrimType DoubleType = float64
goPrimType WorldType = tid' "support.WorldType"

goCastPrimType :
  Expression e =>
  e ->
  PrimType ->
  TypeAssertExpression (CastExpression TypeIdentifier e) TypeIdentifier
goCastPrimType e t = supportCast e $ goPrimType t

goOp pr (Add ty) [x,y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerAdd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /+/ goCastPrimType y' ty
goOp pr (Sub ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerSub"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /-/ goCastPrimType y' ty
goOp pr (Mul ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerMul"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /*/ goCastPrimType y' ty
goOp pr (Div ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerDiv"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /// goCastPrimType y' ty
goOp pr (Mod ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerMod"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /%/ goCastPrimType y' ty
goOp pr (Neg ty) [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp ifn = pr.support "IntegerNeg"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x']
        _ => MkGoExp $ minus' $ goCastPrimType x' ty
goOp pr (ShiftL ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerShiftL"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /<</ goCastPrimType y' ty
goOp pr (ShiftR ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerShiftR"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty />>/ goCastPrimType y' ty
goOp pr (BAnd ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBAnd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /&/ goCastPrimType y' ty
goOp pr (BOr ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /|/ goCastPrimType y' ty
goOp pr (BXOr ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerBXOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /^/ goCastPrimType y' ty
goOp pr (LT ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerLT"
      MkGoExp boolFn = pr.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /</ goCastPrimType y' ty]
goOp pr (LTE ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerLTE"
      MkGoExp boolFn = pr.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /<=/ goCastPrimType y' ty]
goOp pr (EQ ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerEQ"
      MkGoExp boolFn = pr.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /==/ goCastPrimType y' ty]
goOp pr (GTE ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerGTE"
      MkGoExp boolFn = pr.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty />=/ goCastPrimType y' ty]
goOp pr (GT ty) [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp ifn = pr.support "IntegerGT"
      MkGoExp boolFn = pr.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty />/ goCastPrimType y' ty]
goOp pr StrLength [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (id_ "len") [supportCast x' string]
goOp pr StrHead [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ (supportCast x' string) `index` intL 0
goOp pr StrTail [x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ sliceL (supportCast x' string) $ intL 1
goOp pr StrIndex [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
  in MkGoExp $ supportCast x' string `index` (supportCast y' int)
goOp pr StrCons [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp strCons = pr.support "StrCons"
  in MkGoExp $ call strCons [x',y']
goOp pr StrAppend [x, y] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
  in MkGoExp $ supportCast x' string /+/ supportCast y' string
goOp pr StrReverse [x] =
  let MkGoExp x' = goExp pr x
      MkGoExp strReverse = pr.support "StrReverse"
  in MkGoExp $ call strReverse [x']
goOp pr StrSubstr [x, y, z] =
  let MkGoExp x' = goExp pr x
      MkGoExp y' = goExp pr y
      MkGoExp z' = goExp pr z
      MkGoExp strSubstr = pr.support "StrSubstr"
  in MkGoExp $ call strSubstr [x', y', z']
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
  in MkGoExp $ call fn [supportCast x' $ goPrimType pty1]

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
    category CharType = "Char"
    category DoubleType = "Double"
    category WorldType = "World"

goOp pr BelieveMe [_,_,x] = goExp pr x
goOp pr Crash [_,x] =
  let MkGoExp x' = goExp pr x
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] [ expr $ call (id_ "panic") [x'] ]) []

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
      MkGoExp equals = pr.support "IntegerEQ"
  in (case_ [ (call equals [id_ v, c']) /==/ intL 1 ] [ return [ exp' ] ]) :: goIntegerConstAlt pr v alts def

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
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goIntegerConstAlt pr v alts def
      in [ switchS ([id_ v] /:=/ [supportCast exp' $ tid' "support.IntegerType"]) (boolL True) alts' ]

idArgList : List Identifier -> GoExpArgs
idArgList (x::xs) =
  let MkGoExpArgs xs' = idArgList xs
  in MkGoExpArgs (x::xs')
idArgList [] = MkGoExpArgs []

goConAlt : PackageResolver -> String -> List NamedConAlt -> Maybe NamedCExp -> Int -> GoCaseStmtList
goConAlt pr _ [] (Just def) _ =
  let MkGoExp defc = goExp pr def
  in [ default_ [ return [ defc ] ] ]
goConAlt _ _ [] Nothing _ =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goConAlt pr v ((MkNConAlt name _ mTag allArgs exp) :: alts) def n =
  let MkGoStmts stmts@(_::_) = fromGoStmtList $ goConAltBody allArgs 0
        | MkGoStmts empty => []
      caseNum = fromMaybe n mTag
  in (case_ [ intL caseNum ] stmts) :: goConAlt pr v alts def (n+1)
  where
    goConAltBody : List Core.Name.Name -> Int -> GoStmtList
    goConAltBody [] _ =
      case allArgs of
        [] =>
          let MkGoExp exp' = goExp pr exp
          in [ return [ exp' ] ]
        _ =>
          let MkGoStmts stmt = fromGoStmtList $ goStatement pr exp
              allArgs' = map (value . goName) allArgs
              MkGoExpArgs goArgs = idArgList $ map id_ allArgs'
          in [ return
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
goStatement pr exp@(NmLet _ _ _ _) = lets exp []
  where
    lets : NamedCExp -> List String -> GoStmtList
    lets (NmLet _ n val x) vs =
      let MkGoExp val' = goExp pr val
          name = value $ goName n
      in decl (vars [ var [id_ name] (tid' "any") [val'] ]) :: lets x (name :: vs)
    lets exp vs =
      let argNames = reverse vs
          MkGoExpArgs args = idArgList $ map id_ argNames
          MkGoStmts body = fromGoStmtList $ goStatement pr exp
      in [ return [ call (funcL [fields argNames $ tid' "any"] [fieldT $ tid' "any"] body) args ] ]
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

record Foreign where
  constructor MkForeign
  ccs : List String
  args : List CFType
  type : CFType

data Definition : Type where
  FN : Function -> Definition
  FFI : Foreign -> Definition

namedDefToFFIDef : (Core.Name.Name, FC, NamedDef) -> Maybe (Core.Name.Name, Definition)
namedDefToFFIDef (name, _, (MkNmForeign ccs args type)) = Just $ (name, FFI $ MkForeign ccs args type)
namedDefToFFIDef _ = Nothing

functionToFNDef : Function -> (Core.Name.Name, Definition)
functionToFNDef fn = (fn.name, FN fn)

goDefs :
  {auto s : Ref Decls GoDeclList} ->
  PackageResolver ->
  (Go.Name, Definition) ->
  Core ()
goDefs pr (n, nd) = defs nd
  where
    defs : Definition -> Core ()
    defs (FN $ MkFunction _ [] exp) = do
      let MkGoStmts sts = fromGoStmtList $ goStatement pr exp
          fnDecl = docs [show exp, show n.original] $
                     func n.value [] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FN $ MkFunction _ args exp) = do
      let MkGoStmts sts = fromGoStmtList $ goStatement pr exp
          fnDecl = docs [show exp, show n.original] $
                     func n.value [fields (map (value . goName) args) $ tid' "any"] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FFI $ MkForeign _ [] _) = do
      let MkGoExp fn = pr.support $ capitalize n.value
          fnDecl = func n.value [] [fieldT $ tid' "any"]
                    [ return [call fn []]]
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FFI $ MkForeign _ args _) = do
      let args' = [ "v"++show v | v <- [0..cast (length args) - 1]]
          MkGoExp fn = pr.support $ capitalize n.value
          MkGoExpArgs as = idArgList $ map id_ args'
          fnDecl = func n.value [fields args' $ tid' "any"] [fieldT $ tid' "any"]
                    [ return [call fn as] ]
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()

namespace GoImports

  export
  goImportDefs :
    (moduleName : String) ->
    List (Go.Name, Definition) ->
    Imports

  goImportDef :
    (moduleName : String) ->
    (Go.Name, Definition) ->
    Imports

  export
  goImportExp :
    (moduleName : String)->
    NamedCExp ->
    Imports

  goImportOp :
    (moduleName : String)->
    { 0 arity : _ } ->
    PrimFn arity ->
    Vect arity NamedCExp ->
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

  goImportDef mod (n, (FN fn)) = goImportExp mod fn.body
  goImportDef mod (n, (FFI ffi)) = addImport (importForSupport mod) empty -- TODO parse imports from FFI

  goImportExp mod (NmLocal fc n) = empty
  goImportExp mod (NmRef fc n) = let name = goName n in addImport (importForProject mod name.location) empty
  goImportExp mod (NmLam fc x y) = goImportExp mod y
  goImportExp mod (NmLet fc x y z) = merge (goImportExp mod y) $ goImportExp mod z
  goImportExp mod (NmApp fc x xs) = foldl (\acc => merge acc . goImportExp mod) (goImportExp mod x) xs
  goImportExp mod (NmCon fc n x tag xs) = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
  goImportExp mod (NmOp fc f xs) = goImportOp mod f xs
  goImportExp mod (NmExtPrim fc p xs) = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
  goImportExp mod (NmForce fc lz x) = goImportExp mod x
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
  goImportExp mod (NmPrimVal fc WorldVal) = addImport (importForSupport mod) empty
  goImportExp mod (NmPrimVal fc cst) = empty
  goImportExp mod (NmErased fc) = empty
  goImportExp mod (NmCrash fc str) = empty

  addSupportForOp :
    (moduleName : String) ->
    { 0 arity : _ } ->
    Vect arity NamedCExp ->
    Imports
  addSupportForOp mod xs = addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs

  goImportOp mod (Add IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (Sub IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (Mul IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (Div IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (Mod IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (Neg IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (ShiftL IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (ShiftR IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (BAnd IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (BOr IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (BXOr IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (LT IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (LTE IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (EQ IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (GTE IntegerType) xs = addSupportForOp mod xs
  goImportOp mod (GT IntegerType) xs = addSupportForOp mod xs
  goImportOp mod StrReverse xs = addSupportForOp mod xs
  goImportOp mod DoubleExp xs = addSupportForOp mod xs
  goImportOp mod DoubleLog xs = addSupportForOp mod xs
  goImportOp mod DoublePow xs = addSupportForOp mod xs
  goImportOp mod DoubleSin xs = addSupportForOp mod xs
  goImportOp mod DoubleCos xs = addSupportForOp mod xs
  goImportOp mod DoubleTan xs = addSupportForOp mod xs
  goImportOp mod DoubleASin xs = addSupportForOp mod xs
  goImportOp mod DoubleACos xs = addSupportForOp mod xs
  goImportOp mod DoubleATan xs = addSupportForOp mod xs
  goImportOp mod DoubleSqrt xs = addSupportForOp mod xs
  goImportOp mod DoubleFloor xs = addSupportForOp mod xs
  goImportOp mod DoubleCeiling xs = addSupportForOp mod xs
  goImportOp mod (Cast pty pty1) xs = addSupportForOp mod xs
  goImportOp mod _ xs = foldl (\acc => merge acc . goImportExp mod) empty xs

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
          | False => MkGoExp $ id_ n.value
        i = importForProject moduleName n.location
        True = i /= currentImport
          | False => MkGoExp $ id_ n.value
        package = packageForImport i is
    in MkGoExp $ id_ package /./ n.value

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
  (moduleName : String) ->
  (List1 (Go.Name, Definition)) ->
  Core (Maybe String)
goFile outDir moduleName defs = do
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

goMainFile :
  (outDir : String) ->
  (outFile : String) ->
  (moduleName : String) ->
  NamedCExp ->
  Core (Maybe String)
goMainFile outDir outFile moduleName exp = do
  let currentImport = importForMain moduleName
      imports = goImportExp moduleName exp
      packageResolver = MkPackageResolver
                          { project = goRef moduleName currentImport imports
                          , support = goSupport moduleName imports
                          }

  let MkGoExp exp' = goExp packageResolver exp
      src = Go.file (outFile ++ ".go") (package "main") (goImportSpecList currentImport imports)
              [ func "main" [] void [expr exp'] ]

  result <- coreLift $ printFile outDir src
  case result of
    Right () => pure Nothing
    Left e => pure $ Just $ show e

getGrouppedDefs :
  List (Core.Name.Name, Definition) ->
  List (List1 (Go.Name, Definition))
getGrouppedDefs defs =
  groupBy ((==) `on` locationOf)
    $ sortBy (compare `on` locationOf)
    $ map (mapFst goName) defs
  where
    locationOf : (Go.Name, _) -> Location
    locationOf = location . fst

getGoModule : List String -> String -> Core String
getGoModule directives outFile = do
  let Nothing = getFirstArg directives "module"
              | Just mod => pure mod
  pure outFile

Go : CG
Go = Other "go"

copySupportFiles :
  {auto c : Ref Ctxt Defs} ->
  (outDir : String) ->
  Core ()
copySupportFiles outDir = do
  let supportPath = outDir </> "_gen" </> "idris2" </> "support"
  ensureDirectoryExists supportPath
  for_ Gen.files $ \(fname, support) => do
    content <- readDataFile fname `catch` const (pure support)
    writeFile (supportPath </> fname) content

export
compileGo :
  {auto c : Ref Ctxt Defs} ->
  (outDir : String) ->
  (outFile : String) ->
  List (Core.Name.Name, FC, NamedDef) ->
  NamedCExp ->
  Core (Maybe String)
compileGo outDir outFile defs exp = do

  ds <- getDirectives Go
  moduleName <- getGoModule ds outFile

  copySupportFiles outDir

  let ffiDefs = mapMaybe namedDefToFFIDef defs
      fnDefs = map functionToFNDef $ TailRec.functions goTailRecName defs
      grouppedDefs = getGrouppedDefs $ fnDefs ++ ffiDefs
  traverse_ (goFile outDir moduleName) grouppedDefs

  _ <- goMainFile outDir outFile moduleName exp

  Just _ <- GoC.compileProgram ds moduleName outDir outFile
    | Nothing => pure $ Just "go compilation failed"

  pure Nothing

