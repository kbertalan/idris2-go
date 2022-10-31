module Go.AST.Combinators

import public Data.List
import public Data.List1
import public Data.List.Quantifiers
import public Go.AST
import public Go.Token

export
data Package = MkPackage String

export
package : String -> Package
package = MkPackage

export
identifier :
  String ->
  Identifier
identifier name = MkIdentifier Nothing name

export
file :
  { ds : List Type } ->
  { auto 0 ok : All Declaration ds } ->
  (name : String) ->
  Package ->
  List ImportSpec ->
  HList ds ->
  File ds
file name (MkPackage pkg) imports decls = MkFile Nothing Nothing (identifier pkg) decls Nothing Nothing imports [] []

export
string :
  String ->
  BasicLiteral
string str = MkBasicLiteral Nothing MkString str

export
int :
  Int ->
  BasicLiteral
int i = MkBasicLiteral Nothing MkInt $ show i

export
import' :
  (path : String) ->
  ImportSpec
import' path = MkImportSpec Nothing Nothing (string path) Nothing Nothing

export
void : All Field []
void = []

export
param :
  Expression t =>
  String ->
  t ->
  Field t
param f t = MkField Nothing [identifier f] (Just t) Nothing Nothing

export
params :
  Expression t =>
  List String ->
  t ->
  Field t
params fs t = MkField Nothing (identifier <$> fs) (Just t) Nothing Nothing

export
type :
  Expression t =>
  t ->
  Field t
type t = MkField Nothing [] (Just t) Nothing Nothing

export
fieldList :
  All Field xs ->
  FieldList xs
fieldList xs = MkFieldList Nothing xs Nothing

export
funcType :
  All Field ts ->
  All Field ps ->
  All Field rs ->
  FunctionType ts ps rs
funcType ts ps rs = MkFunctionType Nothing (fieldList ts) (fieldList ps) (fieldList rs)

export
block :
  { auto 0 ok : All Statement sts } ->
  HList sts ->
  BlockStatement sts
block sts = MkBlockStatement Nothing sts Nothing

export
func :
  (name : Identifier) ->
  All Field ps ->
  All Field rs ->
  { 0 sts : List Type } ->
  { auto 0 ok : All Statement sts } ->
  HList sts ->
  FuncDeclaration [] [] ps rs sts
func name ps rs sts = MkFuncDeclaration Nothing (fieldList []) name (funcType [] ps rs) (block sts)

export
vars :
  NonEmpty es =>
  All Specification es =>
  HList es ->
  GenericDeclaration MkVar es
vars es = MkGenericDeclaration Nothing Nothing Var Nothing es Nothing

export
var :
  Expression t =>
  All Expression es =>
  List1 Identifier ->
  Maybe t ->
  HList es ->
  ValueSpec t es
var is t es = MkValueSpec Nothing is t es Nothing

export
expr : Expression e => e -> ExpressionStatement e
expr e = MkExpressionStatement e

export
decl : Declaration d => d -> DeclarationStatement d
decl d = MkDeclarationStatement d

export
return :
  All Expression es =>
  HList es ->
  ReturnStatement es
return es = MkReturnStatement Nothing es

export
call :
  Expression fn =>
  fn ->
  { 0 args : List Type } ->
  { auto 0 argsOk : All Expression args } ->
  HList args ->
  CallExpression fn args BadExpression
call fn args = MkCallExpression fn Nothing args Nothing Nothing

export
(/./) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/./) e1 e2 = MkBinaryExpression e1 Nothing MkPeriod e2

infixl 7 /./

export
(/+/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/+/) e1 e2 = MkBinaryExpression e1 Nothing MkAdd e2

export
(/-/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/-/) e1 e2 = MkBinaryExpression e1 Nothing MkSub e2

export
(/*/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/*/) e1 e2 = MkBinaryExpression e1 Nothing MkMul e2

export
(///) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(///) e1 e2 = MkBinaryExpression e1 Nothing MkQuo e2

infixl 6 /+/, /-/
infixl 5 /*/, ///

export
(/:=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/:=/) ls rs = MkAssignmentStatement ls Nothing MkDefine rs

export
(/=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/=/) ls rs = MkAssignmentStatement ls Nothing MkAssign rs

infix 7 /:=/, /=/
