module Go.AST.Combinators

import public Data.List.Quantifiers
import public Go.AST
import Go.Token

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
import' :
  (path : String) ->
  ImportSpec
import' path = MkImportSpec Nothing Nothing (string path) Nothing Nothing

export
void : All Field []
void = []

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
  { auto 0 stsOk1 : All Statement sts } ->
  { auto 0 stsOk2 : Statement (BlockStatement sts) } ->
  HList sts ->
  FuncDeclaration [] [] ps rs sts
func name ps rs sts = MkFuncDeclaration Nothing (fieldList []) name (funcType [] ps rs) (block sts)

export
expr : Expression e => e -> ExpressionStatement e
expr e = MkExpressionStatement e

export
call :
  Expression fn =>
  fn ->
  { 0 args : List Type } ->
  { auto 0 argsOk : All Expression args } ->
  HList args ->
  CallExpression fn args BadExpression
call fn args = MkCallExpression fn Nothing args Nothing Nothing
