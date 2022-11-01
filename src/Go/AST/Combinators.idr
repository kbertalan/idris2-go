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
identifier name = MkIdentifier name

export
file :
  { ds : List Type } ->
  { auto 0 ok : All Declaration ds } ->
  (name : String) ->
  Package ->
  List ImportSpec ->
  HList ds ->
  File ds
file name (MkPackage pkg) imports decls = MkFile Nothing (identifier pkg) decls imports [] []

export
string :
  String ->
  BasicLiteral
string str = MkBasicLiteral MkString str

export
int :
  Int ->
  BasicLiteral
int i = MkBasicLiteral MkInt $ show i

export
bool :
  Bool ->
  BasicLiteral
bool b = MkBasicLiteral MkIdentifier $ case b of
                                                True => "true"
                                                False => "false"

export
import' :
  (path : String) ->
  ImportSpec
import' path = MkImportSpec Nothing Nothing (string path) Nothing

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
fieldList xs = MkFieldList xs

export
funcType :
  All Field ts ->
  All Field ps ->
  All Field rs ->
  FunctionType ts ps rs
funcType ts ps rs = MkFunctionType (fieldList ts) (fieldList ps) (fieldList rs)

export
block :
  { auto 0 ok : All Statement sts } ->
  HList sts ->
  BlockStatement sts
block sts = MkBlockStatement sts

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
vars es = MkGenericDeclaration Nothing Var es

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
return es = MkReturnStatement es

export
call :
  Expression fn =>
  fn ->
  { 0 args : List Type } ->
  { auto 0 argsOk : All Expression args } ->
  HList args ->
  CallExpression fn args BadExpression
call fn args = MkCallExpression fn args Nothing

export
(/./) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/./) e1 e2 = MkBinaryExpression e1 MkPeriod e2

infixl 7 /./

export
(/+/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/+/) e1 e2 = MkBinaryExpression e1 MkAdd e2

export
(/-/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/-/) e1 e2 = MkBinaryExpression e1 MkSub e2

export
(/*/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/*/) e1 e2 = MkBinaryExpression e1 MkMul e2

export
(///) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(///) e1 e2 = MkBinaryExpression e1 MkQuo e2

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
(/:=/) ls rs = MkAssignmentStatement ls MkDefine rs

export
(/=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/=/) ls rs = MkAssignmentStatement ls MkAssign rs

infix 7 /:=/, /=/
