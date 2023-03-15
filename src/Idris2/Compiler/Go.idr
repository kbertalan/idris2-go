module Idris2.Compiler.Go

import Compiler.ES.TailRec

import Core.CompileExpr
import Core.Context
import Core.Context.Log
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

import Idris2.Compiler.Go.FFI
import Idris2.Compiler.Go.GoC
import Idris2.Compiler.Go.Import
import Idris2.Compiler.Go.Name as Go
import Idris2.Compiler.Go.Support.Gen

import Libraries.Utils.Path

%hide Core.Core.cond

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

namespace Usage

  export
  used : Core.Name.Name -> NamedCExp -> Bool

  usedArgs : Core.Name.Name -> List NamedCExp -> Bool
  usedArgsV : {0 arity : _} -> Core.Name.Name -> Vect arity NamedCExp -> Bool
  usedConAlts : Core.Name.Name -> List NamedConAlt -> Bool
  usedConstAlts : Core.Name.Name -> List NamedConstAlt -> Bool

  used name (NmLocal _ n) = name == n
  used name (NmRef _ n) = name == n
  used name (NmLam _ x exp) = name /= x && used name exp
  used name (NmLet _ x def exp) = name /= x && (used name def || used name exp)
  used name (NmApp _ x xs) = used name x || usedArgs name xs
  used name (NmCon _ n _ _ xs) = usedArgs name xs
  used name (NmOp _ f xs) = usedArgsV name xs
  used name (NmExtPrim _ p xs) = usedArgs name xs
  used name (NmForce _ _ exp) = used name exp
  used name (NmDelay _ _ exp) = used name exp
  used name (NmConCase _ condition [MkNConAlt _ _ _ xs exp] Nothing) =
    (used name condition && any (flip used exp) xs) || used name exp
  used name (NmConCase _ exp alts mDef) = used name exp || usedConAlts name alts || fromMaybe False (used name <$> mDef) 
  used name (NmConstCase _ exp alts mDef) = used name exp || usedConstAlts name alts || fromMaybe False (used name <$> mDef)
  used name (NmPrimVal _ cst) = False
  used name (NmErased _) = False
  used name (NmCrash _ str) = False

  usedArgs name [] = False
  usedArgs name (x :: xs) = used name x || usedArgs name xs

  usedArgsV name [] = False
  usedArgsV name (x :: xs) = used name x || usedArgsV name xs

  usedConAlts name [] = False
  usedConAlts name (MkNConAlt _ _ _ _ exp :: xs) = used name exp || usedConAlts name xs

  usedConstAlts name [] = False
  usedConstAlts name (MkNConstAlt _ exp :: xs) = used name exp || usedConstAlts name xs

record Context where
  constructor MkContext
  project : Go.Name -> GoExp
  support : String -> GoExp
  counter : Int
  returns : Bool

(.nextCounter) : Go.Context -> (Int, Go.Context)
(.nextCounter) ctx = (ctx.counter, { counter $= (+1) } ctx )

tcVarName : String
tcVarName = "__tc_var"

tcArgName : String
tcArgName = "__tc_arg"

tcLabelName : String
tcLabelName = "__tc_label"

goExpArgs : Go.Context -> List NamedCExp -> GoExpArgs
goExp : Go.Context -> NamedCExp -> GoExp
goStatement : Go.Context -> NamedCExp -> GoStmtList
goOp : {0 arity : Nat } -> Go.Context -> PrimFn arity -> Vect arity NamedCExp -> GoExp
goConCase : Go.Context -> NamedCExp -> List NamedConAlt -> Maybe NamedCExp -> GoStmtList
goConstCase : Go.Context -> NamedCExp -> List NamedConstAlt -> Maybe NamedCExp -> GoStmtList

goExpArgs _ [] = MkGoExpArgs []
goExpArgs ctx (x::xs) =
  let MkGoExpArgs {ts} {es} {ps} xs' = goExpArgs ctx xs
      MkGoExp {t} {e} {p} x' = goExp ({ returns := True} ctx) x
  in MkGoExpArgs {ts=t::ts} {es=e::es} {ps=p::ps} $ x' :: xs'

isInt64 : Integer -> Bool
isInt64 i = i <= 9223372036854775807 && i >= -9223372036854775808

goPrimConst : Go.Context -> Constant -> GoExp
goPrimConst ctx (I i) = MkGoExp $ intL i
goPrimConst ctx (I8 i) = MkGoExp $ cast_ int8 $ MkBasicLiteral MkInt $ show i
goPrimConst ctx (I16 i) = MkGoExp $ cast_ int16 $ MkBasicLiteral MkInt $ show i
goPrimConst ctx (I32 i) = MkGoExp $ cast_ int32 $ MkBasicLiteral MkInt $ show i
goPrimConst ctx (I64 i) = MkGoExp $ cast_ int64 $ MkBasicLiteral MkInt $ show i
goPrimConst ctx (BI i) =
  if isInt64 i
    then
      MkGoExp $ cast_ int64 $ MkBasicLiteral MkInt $ show i
    else
      let MkGoExp fn = ctx.support "IntegerLiteral"
      in MkGoExp $ call fn [stringL $ show i]
goPrimConst ctx (B8 m) = MkGoExp $ cast_ uint8 $ MkBasicLiteral MkInt $ show m
goPrimConst ctx (B16 m) = MkGoExp $ cast_ uint16 $ MkBasicLiteral MkInt $ show m
goPrimConst ctx (B32 m) = MkGoExp $ cast_ uint32 $ MkBasicLiteral MkInt $ show m
goPrimConst ctx (B64 m) = MkGoExp $ cast_ uint64 $ MkBasicLiteral MkInt $ show m
goPrimConst ctx (Str str) = MkGoExp $ stringL str
goPrimConst ctx (Ch c) = MkGoExp $ charL c
goPrimConst ctx (Db dbl) = MkGoExp $ floatL dbl
goPrimConst ctx (PrT pty) = 
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
  in ctx.support tyName
goPrimConst ctx WorldVal =
  let MkGoExp newWorld = ctx.support "NewWorld"
  in MkGoExp $ call newWorld []

goExp _ (NmLocal _ n) = MkGoExp $ id_ $ value $ goName n
goExp ctx (NmRef _ n) = ctx.project $ goName n
goExp ctx (NmLam _ n exp) =
  let MkGoStmts x = fromGoStmtList $ goStatement ctx exp
  in MkGoExp $ funcL [fields [value $ goName n] $ tid' "any"] [fieldT $ tid' "any"] x
goExp ctx exp@(NmLet _ n val x) =
  let MkGoStmts stmts = fromGoStmtList $ goStatement ctx exp
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] stmts) []
goExp ctx (NmApp _ fn@(NmRef _ _) args) =
  let MkGoExp fn' = goExp ctx fn
      MkGoExpArgs args' = goExpArgs ctx args
  in MkGoExp $ call fn' args'
goExp ctx (NmApp _ fn []) =
  let MkGoExp fn' = goExp ctx fn
  in MkGoExp $ call (typeAssert fn' (func' [] [fieldT $ tid' "any"])) []
goExp ctx (NmApp _ fn args) =
  let MkGoExp fn' = goExp ctx fn
      MkGoExpArgs args' = goExpArgs ctx args
  in MkGoExp $ call (typeAssert fn' (func' [fields ["a" ++ show i | i <- [1..length args]] $ tid' "any"] [fieldT $ tid' "any"])) args'
goExp ctx (NmCon fc n UNIT tag xs) =
  MkGoExp $ id_ "nil"
goExp ctx (NmCon fc n NOTHING tag xs) =
  MkGoExp $ id_ "nil"
goExp ctx (NmCon fc n JUST tag xs) =
  let MkGoExp con = ctx.support "ConstructorPtr"
      MkGoExpArgs args = goExpArgs ctx xs
      tag' = fromMaybe (-1) tag
  in MkGoExp $ call con $ intL tag' :: args
goExp ctx (NmCon fc n x tag xs) =
  let MkGoExp con = ctx.support "Constructor"
      MkGoExpArgs args = goExpArgs ctx xs
      tag' = fromMaybe (-1) tag
  in MkGoExp $ call con $ intL tag' :: args
goExp ctx exp@(NmOp _ f xs) = goOp ctx f xs
goExp ctx (NmExtPrim _ p xs) =
  let name = goName p
      MkGoExp fn = ctx.support $ name.location.package ++ "_" ++ name.value
      MkGoExpArgs args = goExpArgs ctx xs
  in MkGoExp $ call fn args
goExp ctx (NmForce fc lz x) =
  let MkGoExp x' = goExp ctx x
  in MkGoExp $ call (typeAssert (paren x') (func' [] [fieldT $ tid' "any"])) []
goExp ctx (NmDelay fc lz x) =
  let MkGoExp x' = goExp ctx x
      MkGoExp delay = ctx.support "Delay"
  in MkGoExp $ call delay [funcL [] [fieldT $ tid' "any"] [ return [ x' ] ]]
goExp ctx (NmConCase fc sc alts x) =
  let MkGoStmts stmts = fromGoStmtList $ goConCase ctx sc alts x
  in MkGoExp $ call (funcL [] [fieldT $ tid' "any"] stmts) []
goExp ctx (NmConstCase fc sc alts x) =
  let MkGoStmts stmts = fromGoStmtList $ goConstCase ctx sc alts x
  in MkGoExp $ call (funcL [] [fieldT $ tid' "any"] stmts) []
goExp ctx (NmPrimVal _ c) = goPrimConst ctx c
goExp ctx exp@(NmErased fc) =
  let MkGoStmts x = fromGoStmtList $ goStatement ctx exp
  in MkGoExp $ funcL [] [fieldT $ tid' "any"] x
goExp ctx exp@(NmCrash fc str) =
  let MkGoStmts x = fromGoStmtList $ goStatement ctx exp
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
goPrimType CharType = rune
goPrimType DoubleType = float64
goPrimType WorldType = tid' "support.WorldType"

goCastPrimType :
  Expression e =>
  e ->
  PrimType ->
  TypeAssertExpression (CastExpression TypeIdentifier e) TypeIdentifier
goCastPrimType e t = supportCast e $ goPrimType t

goOp ctx (Add ty) [x,y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerAdd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /+/ goCastPrimType y' ty
goOp ctx (Sub ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerSub"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /-/ goCastPrimType y' ty
goOp ctx (Mul ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerMul"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /*/ goCastPrimType y' ty
goOp ctx (Div ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerDiv"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /// goCastPrimType y' ty
goOp ctx (Mod ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerMod"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /%/ goCastPrimType y' ty
goOp ctx (Neg ty) [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp ifn = ctx.support "IntegerNeg"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x']
        _ => MkGoExp $ minus' $ goCastPrimType x' ty
goOp ctx (ShiftL ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerShiftL"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /<</ goCastPrimType y' ty
goOp ctx (ShiftR ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerShiftR"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty />>/ goCastPrimType y' ty
goOp ctx (BAnd ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerBAnd"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /&/ goCastPrimType y' ty
goOp ctx (BOr ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerBOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /|/ goCastPrimType y' ty
goOp ctx (BXOr ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerBXOr"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ goCastPrimType x' ty /^/ goCastPrimType y' ty
goOp ctx (LT ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerLT"
      MkGoExp boolFn = ctx.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /</ goCastPrimType y' ty]
goOp ctx (LTE ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerLTE"
      MkGoExp boolFn = ctx.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /<=/ goCastPrimType y' ty]
goOp ctx (EQ ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerEQ"
      MkGoExp boolFn = ctx.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty /==/ goCastPrimType y' ty]
goOp ctx (GTE ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerGTE"
      MkGoExp boolFn = ctx.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty />=/ goCastPrimType y' ty]
goOp ctx (GT ty) [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp ifn = ctx.support "IntegerGT"
      MkGoExp boolFn = ctx.support "BoolAsInt"
  in case ty of
        IntegerType => MkGoExp $ call ifn [x', y']
        _ => MkGoExp $ call boolFn [goCastPrimType x' ty />/ goCastPrimType y' ty]
goOp ctx StrLength [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp strLength = ctx.support "StrLength"
  in MkGoExp $ call strLength [x']
goOp ctx StrHead [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp strHead = ctx.support "StrHead"
  in MkGoExp $ call strHead [x']
goOp ctx StrTail [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp strTail = ctx.support "StrTail"
  in MkGoExp $ call strTail [x']
goOp ctx StrIndex [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp strIndex = ctx.support "StrIndex"
  in MkGoExp $ call strIndex [x',y']
goOp ctx StrCons [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp strCons = ctx.support "StrCons"
  in MkGoExp $ call strCons [x',y']
goOp ctx StrAppend [x, y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
  in MkGoExp $ supportCast x' string /+/ supportCast y' string
goOp ctx StrReverse [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp strReverse = ctx.support "StrReverse"
  in MkGoExp $ call strReverse [x']
goOp ctx StrSubstr [x, y, z] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp z' = goExp ctx z
      MkGoExp strSubstr = ctx.support "StrSubstr"
  in MkGoExp $ call strSubstr [x', y', z']
goOp ctx DoubleExp [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleExp"
  in MkGoExp $ call fn [x']
goOp ctx DoubleLog [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleLog"
  in MkGoExp $ call fn [x']
goOp ctx DoublePow [x,y] =
  let MkGoExp x' = goExp ctx x
      MkGoExp y' = goExp ctx y
      MkGoExp fn = ctx.support "DoublePow"
  in MkGoExp $ call fn [x', y']
goOp ctx DoubleSin [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleSin"
  in MkGoExp $ call fn [x']
goOp ctx DoubleCos [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleCos"
  in MkGoExp $ call fn [x']
goOp ctx DoubleTan [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleTan"
  in MkGoExp $ call fn [x']
goOp ctx DoubleASin [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleASin"
  in MkGoExp $ call fn [x']
goOp ctx DoubleACos [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleACos"
  in MkGoExp $ call fn [x']
goOp ctx DoubleATan [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleATan"
  in MkGoExp $ call fn [x']
goOp ctx DoubleSqrt [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleSqrt"
  in MkGoExp $ call fn [x']
goOp ctx DoubleFloor [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleFloor"
  in MkGoExp $ call fn [x']
goOp ctx DoubleCeiling [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support "DoubleCeiling"
  in MkGoExp $ call fn [x']
goOp ctx (Cast pty1 pty2) [x] =
  let MkGoExp x' = goExp ctx x
      MkGoExp fn = ctx.support $ "Cast" ++ (category pty1) ++ "To" ++ (type pty2)
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

goOp ctx BelieveMe [_,_,x] = goExp ctx x
goOp ctx Crash [_,x] =
  let MkGoExp x' = goExp ctx x
  in MkGoExp $ call (paren $ funcL [] [fieldT $ tid' "any"] [ expr $ call (id_ "panic") [x'] ]) []

goConstAlt : Go.Context -> List NamedConstAlt -> Maybe NamedCExp -> GoCaseStmtList
goConstAlt ctx [] (Just def) =
  let MkGoStmts sts@(_::_) = fromGoStmtList $ goStatement ctx def
                           | _ => []
  in [ default_ sts ]
goConstAlt ctx [] Nothing =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goConstAlt ctx ((MkNConstAlt c exp) :: alts) def =
  let MkGoStmts sts@(_::_) = fromGoStmtList $ goStatement ctx exp
                           | _ => []
      MkGoExp c' = goPrimConst ctx c
  in (case_ [c'] sts) :: goConstAlt ctx alts def

goIntegerConstAlt : Go.Context -> String -> List NamedConstAlt -> Maybe NamedCExp -> GoCaseStmtList
goIntegerConstAlt ctx v [] (Just def) =
  let MkGoStmts sts@(_::_) = fromGoStmtList $ goStatement ctx def
                           | _ => []
  in [ default_ sts ]
goIntegerConstAlt ctx v [] Nothing =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goIntegerConstAlt ctx v ((MkNConstAlt c exp) :: alts) def =
  let MkGoStmts sts@(_::_) = fromGoStmtList $ goStatement ctx exp
                           | _ => []
      MkGoExp c' = goPrimConst ctx c
      MkGoExp equals = ctx.support "IntegerEQ"
  in (case_ [ (call equals [id_ v, c']) /==/ intL 1 ] sts) :: goIntegerConstAlt ctx v alts def

cond : List (Lazy Bool, Lazy a) -> Lazy a -> a
cond [] def = def
cond ((x,y) :: xs) def = if x then y else cond xs def

goConstCase ctx exp alts def = cond [(isIntegerConst alts, goIntegerConstCase)] goDefaultConstCase
  where
    goDefaultConstCase : GoStmtList
    goDefaultConstCase =
      let MkGoExp exp' = goExp ({ returns := True } ctx) exp
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goConstAlt ctx alts def
      in [ switch exp' alts' ]

    isIntegerConst : List NamedConstAlt -> Bool
    isIntegerConst (MkNConstAlt (BI i) _ :: alts) = True
    isIntegerConst _ = False

    goIntegerConstCase : GoStmtList
    goIntegerConstCase =
      let MkGoExp exp' = goExp ctx exp
          v = "__switch_var"
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goIntegerConstAlt ctx v alts def
      in [ switchS ([id_ v] /:=/ [supportCast exp' $ tid' "support.IntegerType"]) (boolL True) alts' ]

idArgList : List Identifier -> GoExpArgs
idArgList (x::xs) =
  let MkGoExpArgs xs' = idArgList xs
  in MkGoExpArgs (x::xs')
idArgList [] = MkGoExpArgs []

goConAlt : Go.Context -> String -> List NamedConAlt -> Maybe NamedCExp -> Int -> GoCaseStmtList
goConAlt ctx _ [] (Just def) _ =
  let MkGoStmts sts@(_::_) = fromGoStmtList $ goStatement ctx def
                           | _ => []
  in [ default_ sts ]
goConAlt _ _ [] Nothing _ =
  [ default_ [ expr $ call (id_ "panic") [ stringL "reaching impossible default case" ] ]]
goConAlt ctx v ((MkNConAlt name _ mTag allArgs exp) :: alts) def n =
  let MkGoStmts stmts@(_::_) = fromGoStmtList $ goConAltBody allArgs 0
                             | _ => []
      caseNum = fromMaybe n mTag
  in (case_ [ intL caseNum ] stmts) :: goConAlt ctx v alts def (n+1)
  where
    goConAltBody : List Core.Name.Name -> Int -> GoStmtList
    goConAltBody [] _ = goStatement ctx exp
    goConAltBody (name :: args) n =
      if used name exp
        then ([ id_ $ value $ goName name ] /:=/ [id_ v /./ "Args" `index` intL n]) :: goConAltBody args (n+1)
        else goConAltBody args (n+1)

goConMaybeAlt : Go.Context -> NamedCExp -> List NamedConAlt -> Maybe NamedCExp -> GoStmtList
goConMaybeAlt ctx exp alts mDef =
  case (alts, mDef) of
    ([MkNConAlt _ NOTHING _ _ body], Nothing) => goStatement ctx body
    ([MkNConAlt _ NOTHING _ _ body], Just def) =>
      let MkGoStmts def' = fromGoStmtList $ goStatement ctx def
          MkGoExp exp' = goExp ({ returns := True } ctx) exp
      in (if_ (exp' /!=/ id_ "nil") def') :: goStatement ctx body
    ([MkNConAlt _ JUST _ [arg] body], Nothing) =>
      let referenced = used arg body
          arg' = value $ goName arg
          body' = goStatement ctx body
          MkGoExp asValuePtr = ctx.support "AsValuePtr"
          MkGoExp exp' = goExp ({ returns := True } ctx) exp
      in if referenced
            then ([ id_ arg' ] /:=/ [ (call asValuePtr [ exp' ]) /./ "Args" `index` intL 0 ]) :: body'
            else body'
    ([MkNConAlt _ JUST _ [arg] body], Just def) => conMaybeAlt arg body def
    ([MkNConAlt _ JUST _ [arg] justBody, MkNConAlt _ NOTHING _ _ nothingBody], _) => conMaybeAlt arg justBody nothingBody
    ([MkNConAlt _ NOTHING _ _ nothingBody, MkNConAlt _ JUST _ [arg] justBody], _) => conMaybeAlt arg justBody nothingBody
    _ => [ expr $ stringL $ "unrecognized maybe case " ++ show ((\(MkNConAlt _ ci _ args _) => show ci ++ show args) <$> alts) ++ " " ++ show (isJust mDef) ]
  where
    conMaybeAlt : Core.Name.Name -> NamedCExp -> NamedCExp -> GoStmtList
    conMaybeAlt arg justBody nothingBody =
      let (c, ctx') = ctx.nextCounter
          v = "__if_maybe_var_" ++ show c
          MkGoExp asValuePtr = ctx'.support "AsValuePtr"
          MkGoExp exp' = goExp ({ returns := True } ctx) exp
          referenced = used arg justBody
          arg' = value $ goName arg
          justBody' = goStatement ctx' justBody
          MkGoStmts nothingBody' = fromGoStmtList $ goStatement ctx' nothingBody
      in ([id_ v] /:=/ [call asValuePtr [exp']])
         :: (if_ (id_ v /==/ id_ "nil") nothingBody')
         :: ( if referenced
                then ([ id_ arg' ] /:=/ [ id_ v /./ "Args" `index` intL 0]) :: justBody'
                else justBody'
            )

goConSingleAlt : Go.Context -> NamedCExp -> List NamedConAlt -> Maybe NamedCExp -> GoStmtList
goConSingleAlt ctx exp [MkNConAlt _ _ _ args body] Nothing =
  if any (flip used body) args
    then
      let (c, ctx') = ctx.nextCounter
          v = "__single_var_" ++ show c
          MkGoExp asValue = ctx'.support "AsValue"
          MkGoExp exp' = goExp ({ returns := True } ctx) exp
      in ([ id_ v ] /:=/ [call asValue [exp']]) :: (generateBody ctx' v 0 args)
    else goStatement ctx body
  where
    generateBody : Go.Context -> String -> Int -> List Core.Name.Name ->  GoStmtList
    generateBody ctx v n [] = goStatement ctx body
    generateBody ctx v n (name :: ns) =
      if used name body
         then ([ id_ $ value $ goName name ] /:=/ [id_ v /./ "Args" `index` intL n])
              :: generateBody ctx v (n+1) ns
         else generateBody ctx v (n+1) ns

goConSingleAlt _ _ alts def = [ expr $ stringL "unrecognized single case" ]

goConCase ctx exp alts def =
  cond [ (isMaybeCon alts, goMaybeConCase)
       , (isSingleAlt alts def, goSingleConCase)
       ] goDefaultConCase
  where
    goDefaultConCase : GoStmtList
    goDefaultConCase =
      let MkGoExp exp' = goExp ({ returns := True} ctx) exp
          v = "__switch_con_var"
          MkGoCaseStmts alts' = fromGoCaseStmtList $ goConAlt ctx v alts def 0
          MkGoExp asValue = ctx.support "AsValue"
      in [ switchS ([id_ v] /:=/ [call asValue [exp']]) (id_ v /./ "Tag") alts' ]

    isMaybeCon : List NamedConAlt -> Bool
    isMaybeCon (MkNConAlt _ JUST _ _ _ :: _) = True
    isMaybeCon (MkNConAlt _ NOTHING _ _ _ :: _) = True
    isMaybeCon _ = False

    goMaybeConCase : GoStmtList
    goMaybeConCase = goConMaybeAlt ctx exp alts def

    isSingleAlt : List NamedConAlt -> Maybe NamedCExp -> Bool
    isSingleAlt [_] Nothing = True
    isSingleAlt _ _ = False

    goSingleConCase : GoStmtList
    goSingleConCase = goConSingleAlt ctx exp alts def

goReturn : Go.Context -> GoExp -> GoStmtList
goReturn ctx (MkGoExp exp) =
  if ctx.returns
    then [ return [ exp ] ]
    else [ expr exp ]

goStatement ctx exp@(NmLocal _ _) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmRef _ _) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmLam _ _ _) = goReturn ctx $ goExp ctx exp
goStatement ctx (NmLet _ n val exp) =
  let MkGoExp val' = goExp ({ returns := True } ctx) val
  in if used n exp
       then ([id_ $ value $ goName n] /:=/ [cast_ (tid' "any") val']) :: goStatement ctx exp
       else expr val' :: goStatement ctx exp
goStatement ctx exp@(NmApp fc x xs) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmCon fc n x tag xs) =
  if isTcDone n || isTcContinue n
    then [ id_ tcVarName ] /=/ [ intL $ fromMaybe (-1) tag ]
         :: [ id_ tcArgName ] /=/ [ id_ tcArgName `sliceH` intL (fromInteger $ natToInteger $ length xs)]
         :: assignValues 0 xs [ continue tcLabelName ]
    else goReturn ctx $ goExp ctx exp
  where
    isTcDone : Core.Name.Name -> Bool
    isTcDone (MN n _) = n == "TcDone"
    isTcDone _ = False

    isTcContinue : Core.Name.Name -> Bool
    isTcContinue (MN n _) = "TcContinue" `isPrefixOf` n
    isTcContinue _ = False

    assignValues : Int -> List NamedCExp -> GoStmtList -> GoStmtList
    assignValues _ [] rest = rest
    assignValues n (exp :: xs) rest =
      let MkGoExp exp' = goExp ({ returns := True} ctx) exp
      in ([ id_ tcArgName `index` intL n ] /=/ [exp']) :: assignValues (n+1) xs rest
goStatement ctx exp@(NmOp fc f xs) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmExtPrim fc p xs) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmForce fc lz x) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmDelay fc lz x) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmConCase fc sc xs x) = goConCase ctx sc xs x
goStatement ctx exp@(NmConstCase fc sc xs x) = goConstCase ctx sc xs x
goStatement ctx exp@(NmPrimVal fc cst) = goReturn ctx $ goExp ctx exp
goStatement ctx exp@(NmErased _) = [ expr $ call (id_ "panic") [stringL "executing erased term"] ]
goStatement ctx exp@(NmCrash _ str) = [ expr $ call (id_ "panic") [stringL str] ]

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

record BuiltInForeign where
  constructor MkBuiltInForeign
  args : List CFType
  type : CFType

record Foreign where
  constructor MkForeign
  imports : List ForeignImport
  expression: String
  args : List CFType
  type : CFType

data Definition : Type where
  FN : Function -> Definition
  TRFN : Int -> List Core.Name.Name -> List NamedConAlt -> Definition
  BIFFI : BuiltInForeign -> Definition
  FFI : Foreign -> Definition

namedDefToFFIDef : (Core.Name.Name, FC, NamedDef) -> Core $ Maybe (Core.Name.Name, Definition)
namedDefToFFIDef (name, _, (MkNmForeign ccs args type)) = do
  let [str] = filter (isPrefixOf "go:") ccs
        | [] => pure $ Just (name, BIFFI $ MkBuiltInForeign args type)
        | xs => throw $ Fatal $ UserError "there are multiple go FFI definitions specified"
      Right (imports, expression) = parseFFI str
        | Left e => throw $ Fatal $ UserError $ "could not parse FFI string: " <+> show e
  pure $ Just $ (name, FFI $ MkForeign imports expression args type)
namedDefToFFIDef _ = pure Nothing

functionToFNDef : Function -> (Core.Name.Name, Definition)
functionToFNDef fn = (fn.name, FN fn)

tailRecFunctionToFNDef : List Function -> Function -> Maybe (Core.Name.Name, Definition)
tailRecFunctionToFNDef tcOptFns fn =
  let Just (tcOptFnName, tag) = tcOptFnFrom fn
        | Nothing => Nothing
      Just (MkFunction _ args tcOptFnDef) = find (nameMatcher tcOptFnName) tcOptFns
        | _ => Nothing
      Just alts = extractAlts tcOptFnDef
        | Nothing => Nothing
  in Just (fn.name, TRFN tag fn.args alts)
  where
    tcOptFnFrom : Function -> Maybe (Core.Name.Name, Int)
    tcOptFnFrom (MkFunction _ _ (NmApp _ (NmRef _ tcName) args)) =
      let [NmRef _ tcOptFnName, initValue] = args | _ => Nothing
          NmCon _ _ _ tag _ = initValue | _ => Nothing
      in if tcName == goTailRecName then Just (tcOptFnName, fromMaybe (-1) tag)
                                    else Nothing
    tcOptFnFrom _ = Nothing

    nameMatcher : Core.Name.Name -> Function -> Bool
    nameMatcher name (MkFunction n _ _) = name == n

    extractAlts : NamedCExp -> Maybe $ List NamedConAlt
    extractAlts (NmConCase _ _ alts Nothing) = Just alts
    extractAlts (NmConCase _ _ _ (Just def)) = Nothing
    extractAlts _ = Nothing

categorizeFns : List Function -> (List Function, List Function, List Function)
categorizeFns fns = go [] [] [] fns
  where
    isTailRecFn : Function -> Bool
    isTailRecFn (MkFunction _ _ (NmApp _ (NmRef _ name) _)) = name == goTailRecName
    isTailRecFn _ = False

    isTcOptName : Core.Name.Name -> Bool
    isTcOptName (MN name _) = name == "$tcOpt"
    isTcOptName _ = False

    isTcOptFn : Function -> Bool
    isTcOptFn (MkFunction name args _) = length args == 1 && isTcOptName name

    go : List Function -> List Function -> List Function -> List Function -> (List Function, List Function, List Function)
    go fnDefs tailRecFns tcOptFns [] = (fnDefs, tailRecFns, tcOptFns)
    go fnDefs tailRecFns tcOptFns (f::fs) =
      let False = isTailRecFn f
            | True => go fnDefs (f::tailRecFns) tcOptFns fs
          False = isTcOptFn f
            | True => go fnDefs tailRecFns (f::tcOptFns) fs
      in go (f::fnDefs) tailRecFns tcOptFns fs

goTailCallFnBody :
  Go.Context ->
  (tag : Int) ->
  (args : List Core.Name.Name) ->
  List NamedConAlt ->
  GoStmtList
goTailCallFnBody ctx tag args alts =
  let MkGoStmts sts = fromGoStmtList $ switchForAlts alts
      cap = capFromAlts alts 1
  in [ id_ tcVarName ] /:=/ [ intL tag ]
     :: [ id_ tcArgName ] /:=/ [ make (array' $ tid' "any") [intL $ fromInteger $ natToInteger $ length args, intL cap] ]
     :: assignValues 0 args 
         [ label tcLabelName $ while (id_ tcVarName /!=/ intL 0) sts
         , return [ id_ tcArgName `index` intL 0 ]
         ]
  where
    deconstructArgs : Int -> List Core.Name.Name -> NamedCExp -> GoStmtList
    deconstructArgs n (name :: ns) exp =
      if used name exp
        then ([ id_ $ value $ goName name ] /:=/ [ id_ tcArgName `index` intL n ]) :: deconstructArgs (n+1) ns exp
        else deconstructArgs (n+1) ns exp
    deconstructArgs _ [] exp = goStatement ({ returns := False } ctx) exp

    whileCases : List NamedConAlt -> GoCaseStmtList
    whileCases (MkNConAlt _ _ mTag args exp :: as) =
      let tag = fromMaybe (-1) mTag
          MkGoStmts body@(_::_) = fromGoStmtList $ deconstructArgs 0 args exp
                                | _ => []
      in (case_ [intL tag] body) :: whileCases as
    whileCases [] = []

    switchForAlts : List NamedConAlt -> GoStmtList
    switchForAlts [] = []
    switchForAlts [MkNConAlt _ _ _ args exp] = deconstructArgs 0 args exp
    switchForAlts alts =
      let MkGoCaseStmts cases = fromGoCaseStmtList $ whileCases alts
      in [ switch (id_ tcVarName) cases ]

    capFromAlts : List NamedConAlt -> Int -> Int
    capFromAlts [] m = m
    capFromAlts (MkNConAlt _ _ _ args _ :: alts) m =
      let l = fromInteger $ natToInteger $ length args
      in capFromAlts alts $ if m < l then l else m

    assignValues : Int -> List Core.Name.Name -> GoStmtList -> GoStmtList
    assignValues _ [] rest = rest
    assignValues n (name :: ns) rest =
      ([ id_ tcArgName `index` intL n ] /=/ [ id_ $ value $ goName name]) :: assignValues (n+1) ns rest

goLog : 
  {auto c : Ref Ctxt Defs} ->
  Nat ->
  String ->
  Core ()
goLog n str =
  log' (mkUnverifiedLogLevel "compiler.go" n) str

goDefs :
  {auto c : Ref Ctxt Defs} ->
  {auto s : Ref Decls GoDeclList} ->
  Go.Context ->
  (Go.Name, Definition) ->
  Core ()
goDefs ctx (n, nd) = defs nd
  where
    defs : Definition -> Core ()
    defs (FN $ MkFunction _ [] exp) = do
      goLog 10 "generate function for: \{show n.value}"
      goLog 20 "generate from expression: \{show exp}"
      let MkGoStmts sts = fromGoStmtList $ goStatement ctx exp
          fnDecl = docs [show exp, show n.original] $
                     func n.value [] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FN $ MkFunction _ args exp) = do
      goLog 10 "generate function for: \{show n.value}"
      goLog 20 "generate from expression: \{show exp}"
      let MkGoStmts sts = fromGoStmtList $ goStatement ctx exp
          fnDecl = docs [show exp, show n.original] $
                     func n.value [fields (map (value . goName) args) $ tid' "any"] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (TRFN tag [] alts) = do
      goLog 10 "generate recursive function for: \{show n.value}"
      goLog 20 "generate from alts: \{show alts}"
      let MkGoStmts sts = fromGoStmtList $ goTailCallFnBody ctx tag [] alts
          fnDecl = docs [show alts, show n.original] $
                     func n.value [] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (TRFN tag args alts) = do
      goLog 10 "generate recursive function for: \{show n.value}"
      goLog 20 "generate from alts: \{show alts}"
      let MkGoStmts sts = fromGoStmtList $ goTailCallFnBody ctx tag args alts
          fnDecl = docs [show alts, show n.original] $
                     func n.value [fields (map (value . goName) args) $ tid' "any"] [fieldT $ tid' "any"] sts
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (BIFFI $ MkBuiltInForeign [] _) = do
      goLog 10 "generate built-in foreign function for: \{show n.value}"
      let MkGoExp fn = ctx.support $ capitalize n.value
          fnDecl = func n.value [] [fieldT $ tid' "any"]
                    [ return [call fn []]]
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (BIFFI $ MkBuiltInForeign args _) = do
      goLog 10 "generate built-in foreign function for: \{show n.value}"
      let args' = [ "v"++show v | v <- [0..cast (length args) - 1]]
          MkGoExp fn = ctx.support $ capitalize n.value
          MkGoExpArgs as = idArgList $ map id_ args'
          fnDecl = func n.value [fields args' $ tid' "any"] [fieldT $ tid' "any"]
                    [ return [call fn as] ]
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FFI $ MkForeign _ exp [] _) = do
      goLog 10 "generate foreign function for: \{show n.value}"
      let fnDecl = func n.value [] [fieldT $ tid' "any"]
                    [ return [call (paren $ id_ exp) []]]
                    |> docs [show n.original]
      decls <- get Decls
      put Decls (fnDecl :: decls)
      pure ()
    defs (FFI $ MkForeign _ exp args _) = do
      goLog 10 "generate foreign function for: \{show n.value}"
      let args' = [ "v"++show v | v <- [0..cast (length args) - 1]]
          MkGoExpArgs as = idArgList $ map id_ args'
          fnDecl = func n.value [fields args' $ tid' "any"] [fieldT $ tid' "any"]
                    [ return [call (paren $ id_ exp) as] ]
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
  goImportDef mod (n, (TRFN _ _ alts)) = foldl (\acc => merge acc . goImportConAlt mod) empty alts
  goImportDef mod (n, (BIFFI _)) = addImport (importForSupport mod) empty
  goImportDef mod (n, (FFI ffi)) = foldl (\acc => merge acc . ffiImport mod) empty (ffi.imports)
    where
      ffiImport : String -> ForeignImport -> Imports
      ffiImport mod MkSupportImport = addImport (importForSupport mod) empty
      ffiImport mod (MkModuleImport pkg path) = addImport (MkImport Project (mod ++ "/" ++ path) pkg) empty
      ffiImport mod (MkPathImport pkg path) = addImport (importForExternal path pkg) empty


  goImportExp mod (NmLocal fc n) = empty
  goImportExp mod (NmRef fc n) = let name = goName n in addImport (importForProject mod name.location) empty
  goImportExp mod (NmLam fc x y) = goImportExp mod y
  goImportExp mod (NmLet fc x y z) = merge (goImportExp mod y) $ goImportExp mod z
  goImportExp mod (NmApp fc x xs) = foldl (\acc => merge acc . goImportExp mod) (goImportExp mod x) xs
  goImportExp mod (NmCon fc n conInfo tag xs) =
    case conInfo of
      UNIT => empty
      NOTHING => empty
      _ => addImport (importForSupport mod) $ foldl (\acc => merge acc . goImportExp mod) empty xs
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
  goImportExp mod (NmPrimVal fc (BI i)) = if isInt64 i then empty else addImport (importForSupport mod) empty
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
  {auto c : Ref Ctxt Defs} ->
  (outDir : String) ->
  (moduleName : String) ->
  (List1 (Go.Name, Definition)) ->
  Core (Maybe String)
goFile outDir moduleName defs = do
  let (name, _) = head defs
  goLog 5 "prepare \{show name.location.fileName}"
  ensureDirectoryExists (outDir </> name.location.dir)
  let currentImport = importForProject moduleName name.location
      imports = goImportDefs moduleName $ forget defs
      ctx = MkContext
              { project = goRef moduleName currentImport imports
              , support = goSupport moduleName imports
              , counter = 0
              , returns = True
              }

  goLog 5 "generate \{show name.location.fileName}"
  _ <- newRef Decls []
  traverse_ (goDefs ctx) $ forget defs

  goDecls <- get Decls
  let MkGoDecls decls = fromGoDecls goDecls
      src = Go.file (name.location.dir </> name.location.fileName) (package name.location.package) (goImportSpecList currentImport imports) decls

  goLog 5 "write \{show name.location.fileName}"
  result <- coreLift $ printFile outDir src
  case result of
    Right () => pure Nothing
    Left e => pure $ Just $ show e

goMainFile :
  {auto c : Ref Ctxt Defs} ->
  (outDir : String) ->
  (outFile : String) ->
  (moduleName : String) ->
  NamedCExp ->
  Core (Maybe String)
goMainFile outDir outFile moduleName exp = do
  let fileName = outFile ++ ".go"
  goLog 5 "prepare \{show fileName}"
  let currentImport = importForMain moduleName
      imports = goImportExp moduleName exp
      ctx = MkContext
              { project = goRef moduleName currentImport imports
              , support = goSupport moduleName imports
              , counter = 0
              , returns = True
              }

  goLog 5 "generate \{show fileName}"
  goLog 10 "generate from: \{show exp}"
  let MkGoExp exp' = goExp ctx exp
      src = Go.file (outFile ++ ".go") (package "main") (goImportSpecList currentImport imports)
              [ func "main" [] void [expr exp'] ]

  goLog 5 "write \{show fileName}"
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
  let ownOutDir = outDir </> outFile ++ "-src"

  copySupportFiles ownOutDir

  ffiDefs <- map catMaybes $ traverse namedDefToFFIDef defs

  let (fnDefs, tailRecFns, tcOptFns) = categorizeFns $ TailRec.functions goTailRecName defs
      fnDefs' = map functionToFNDef fnDefs
  Just tailRecFns' <- pure $ traverse (tailRecFunctionToFNDef tcOptFns) tailRecFns
    | Nothing => pure $ Just "found a tailRec function without tcOpt function"
  let grouppedDefs = getGrouppedDefs $ fnDefs' ++ tailRecFns' ++ ffiDefs
  traverse_ (goFile ownOutDir moduleName) grouppedDefs

  _ <- goMainFile ownOutDir outFile moduleName exp

  goLog 5 "compile \{show outFile}"
  Just _ <- GoC.compileProgram ds moduleName ownOutDir $ ".." </> outFile
    | Nothing => pure $ Just "go compilation failed"

  goLog 5 "completed \{show outFile}"
  pure Nothing

