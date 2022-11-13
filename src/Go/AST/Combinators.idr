module Go.AST.Combinators

import public Data.List
import public Data.List1
import public Data.List.Quantifiers
import public Go.AST
import public Go.Token

export
(|>) : a -> (a -> b) -> b
(|>) a fn = fn a

infixl 1 |>

public export
interface Commentable a where
  setComments : CommentGroup -> a -> a

export
comment : Commentable a => String -> a -> a
comment str = setComments $ MkCommentGroup $ singleton $ MkComment str

export
comments : Commentable a => (cs : List String) -> {auto 0 ok : NonEmpty cs} -> a -> a
comments (x::xs) = setComments $ MkCommentGroup $ map MkComment (x:::xs)

export
implementation All Expression ls => All Expression rs => NonEmpty ls => NonEmpty rs => Commentable (AssignmentStatement ls rs) where
  setComments cg = { comment := Just cg }

export
implementation Expression e => Commentable (ExpressionStatement e) where
  setComments cg = { comment := Just cg }

export
implementation GoType e => All Expression es => Commentable (ValueSpec e es) where
  setComments cg = { comment := Just cg }

public export
interface Documentable a where
  setDocs : CommentGroup -> a -> a

export
doc : Documentable a => String -> a -> a
doc str = setDocs $ MkCommentGroup $ singleton $ MkComment str

export
docs : Documentable a => (ds : List String) -> {auto 0 ok : NonEmpty ds} -> a -> a
docs (d::ds) = setDocs $ MkCommentGroup $ map MkComment (d:::ds)

export
implementation Expression ls => All Expression rs => Documentable (ValueSpec ls rs) where
  setDocs ds = { doc := Just ds }

export
implementation Expression e => Documentable (ExpressionStatement e) where
  setDocs ds = { doc := Just ds }

export
implementation All Expression rs => Documentable (ReturnStatement rs) where
  setDocs ds = { doc := Just ds }

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
id' :
  String ->
  Identifier
id' name = MkIdentifier name

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
float :
  Double ->
  BasicLiteral
float f = MkBasicLiteral MkFloat $ show f

export
exp :
  Double ->
  Int ->
  BasicLiteral
exp f e = MkBasicLiteral MkFloat $ concat [floored, "e", show e]
  where
    floored : String
    floored = if f == floor f then show $ the Int $ cast $ floor f
                              else show f

export
imag :
  Int ->
  BasicLiteral
imag i = MkBasicLiteral MkImag "\{show i}i"

export
bool :
  Bool ->
  BasicLiteral
bool b = MkBasicLiteral MkIdentifier $ case b of
                                        True => "true"
                                        False => "false"

export
composite :
  Expression t =>
  All Expression es =>
  t ->
  HList es ->
  CompositLiteral t es
composite t es = MkCompositLiteral (Just t) es

export
composite' :
  All Expression es =>
  HList es ->
  CompositLiteral BadExpression es
composite' es = MkCompositLiteral Nothing es

export
import' :
  (path : String) ->
  ImportSpec
import' path = MkImportSpec Nothing Nothing (string path) Nothing

export
void : FieldList []
void = []

export
field :
  GoType t =>
  List String ->
  t ->
  Field t
field fs t = MkField Nothing (identifier <$> fs) (Just t) Nothing Nothing

export
field' :
  List String ->
  Field BadType
field' fs = MkField Nothing (identifier <$> fs) (Maybe BadType `the` Nothing) Nothing Nothing

-- Types

export
struct :
  FieldList ts ->
  StructType ts
struct = MkStructType

export
array :
  Expression l =>
  GoType t =>
  l ->
  t ->
  ArrayType l t
array l t = MkArrayType l t

export
func :
  (name : Identifier) ->
  FieldList ps ->
  FieldList rs ->
  { 0 sts : List Type } ->
  { auto 0 ok : All Statement sts } ->
  HList sts ->
  FuncDeclaration [] [] ps rs sts
func name ps rs sts = MkFuncDeclaration Nothing [] name (MkFunctionType [] ps rs) (MkBlockStatement sts)

export
types :
  NonEmpty es =>
  All Specification es =>
  HList es ->
  GenericDeclaration MkType es
types es = MkGenericDeclaration Nothing Type' es

export
type :
  GoType t =>
  String ->
  FieldList ts ->
  t ->
  TypeSpec ts t
type name typeParams t = MkTypeSpec Nothing (id' name) typeParams t Nothing

export
consts :
  NonEmpty es =>
  All Specification es =>
  HList es ->
  GenericDeclaration MkConst es
consts es = MkGenericDeclaration Nothing Const es

export
const' :
  Expression t =>
  All Expression es =>
  (is : List Identifier) ->
  {auto 0 ok : NonEmpty is} ->
  Maybe t ->
  HList es ->
  ValueSpec t es
const' (i::is) t es = MkValueSpec Nothing (i:::is) t es Nothing

export
vars :
  NonEmpty es =>
  All Specification es =>
  HList es ->
  GenericDeclaration MkVar es
vars es = MkGenericDeclaration Nothing Var es

export
var' :
  All Expression es =>
  (is : List Identifier) ->
  {auto 0 ok : NonEmpty is} ->
  HList es ->
  ValueSpec BadType es
var' (i::is) es = MkValueSpec Nothing (i:::is) Nothing es Nothing

export
var :
  GoType t =>
  All Expression es =>
  (is : List Identifier) ->
  {auto 0 ok : NonEmpty is} ->
  t ->
  HList es ->
  ValueSpec t es
var (i::is) t es = MkValueSpec Nothing (i:::is) (Just t) es Nothing

export
block :
  All Statement ts =>
  HList ts ->
  BlockStatement ts
block = MkBlockStatement

export
expr : Expression e => e -> ExpressionStatement e
expr e = MkExpressionStatement Nothing e Nothing

export
decl : Declaration d => d -> DeclarationStatement d
decl d = MkDeclarationStatement d

export
defer :
  CallExpression f as e ->
  DeferStatement f as e
defer c = MkDeferStatement c

export
return :
  All Expression es =>
  HList es ->
  ReturnStatement es
return es = MkReturnStatement Nothing es

export
for' :
  Statement i =>
  Expression c =>
  Statement p =>
  All Statement sts =>
  i ->
  c ->
  p ->
  HList sts ->
  ForStatement i c p sts
for' i c p sts = MkForStatement (Just i) (Just c) (Just p) (MkBlockStatement sts)

export
forever :
  All Statement sts =>
  HList sts ->
  ForStatement BadStatement BadExpression BadStatement sts
forever sts = MkForStatement Nothing Nothing Nothing $ MkBlockStatement sts

export
while :
  Expression c =>
  All Statement sts =>
  c ->
  HList sts ->
  ForStatement BadStatement c BadStatement sts
while c sts = MkForStatement Nothing (Just c) Nothing $ MkBlockStatement sts

export
if' :
  Expression c =>
  All Statement sts =>
  c ->
  HList sts ->
  IfStatement BadStatement c sts BadStatement
if' c sts = MkIfStatement Nothing c (MkBlockStatement sts) Nothing

export
ifs :
  Statement i =>
  Expression c =>
  All Statement sts =>
  i ->
  c ->
  HList sts ->
  IfStatement i c sts BadStatement
ifs i c sts = MkIfStatement (Just i) c (MkBlockStatement sts) Nothing

export
ifE :
  Expression c =>
  All Statement sts =>
  Statement e =>
  c ->
  HList sts ->
  e ->
  IfStatement BadStatement c sts e
ifE c sts e = MkIfStatement Nothing c (MkBlockStatement sts) (Just e)

export
ifsE :
  Statement i =>
  Expression c =>
  All Statement sts =>
  Statement e =>
  i ->
  c ->
  HList sts ->
  e ->
  IfStatement i c sts e
ifsE i c sts e = MkIfStatement (Just i) c (MkBlockStatement sts) (Just e)

export
switchs :
  Statement i =>
  Expression e =>
  All Statement sts =>
  All IsCaseClause sts =>
  i ->
  e ->
  HList sts ->
  SwitchStatement i e sts
switchs i e sts = MkSwitchStatement (Just i) (Just e) (MkBlockStatement sts)

export
switch :
  Expression e =>
  All Statement sts =>
  All IsCaseClause sts =>
  e ->
  HList sts ->
  SwitchStatement BadStatement e sts
switch e sts = MkSwitchStatement Nothing (Just e) (MkBlockStatement sts)

export
switch' :
  All Statement sts =>
  All IsCaseClause sts =>
  HList sts ->
  SwitchStatement BadStatement BadExpression sts
switch' sts = MkSwitchStatement Nothing Nothing (MkBlockStatement sts)

export
case' :
  All Expression es =>
  All Statement sts =>
  NonEmpty sts =>
  HList es ->
  HList sts ->
  CaseClause es sts
case' es sts = MkCaseClause es sts

export
default' :
  All Statement sts =>
  NonEmpty sts =>
  HList sts ->
  CaseClause [] sts
default' sts = MkCaseClause [] sts

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
index :
  Expression e =>
  Expression i =>
  e ->
  i ->
  IndexExpression e i
index e i = MkIndexExpression e i

export
inc :
  Expression e =>
  e ->
  IncDecStatement e MkInc
inc e = MkIncDecStatement e Inc

export
dec :
  Expression e =>
  e ->
  IncDecStatement e MkDec
dec e = MkIncDecStatement e Dec

export
minus' :
  Expression e =>
  e ->
  UnaryExpression e
minus' e = MkUnaryExpression MkSub e

export
(/./) :
  Expression e =>
  e ->
  String ->
  SelectorExpression e
(/./) e f = MkSelectorExpression e $ id' f

export
(/:/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  KeyValueExpression e1 e2
(/:/) e1 e2 = MkKeyValueExpression e1 e2

infixl 3 /./, /:/

export
(/==/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/==/) e1 e2 = MkBinaryExpression e1 MkEql e2

export
(/!=/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/!=/) e1 e2 = MkBinaryExpression e1 MkNotEql e2

export
(/</) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/</) e1 e2 = MkBinaryExpression e1 MkLess e2

export
(/<=/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/<=/) e1 e2 = MkBinaryExpression e1 MkLessThanOrEqual e2

export
(/>/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/>/) e1 e2 = MkBinaryExpression e1 MkGreater e2

export
(/>=/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/>=/) e1 e2 = MkBinaryExpression e1 MkGreaterThanOrEqual e2

infixl 7 /==/, /!=/, /</, /<=/, />/, />=/

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

infixl 8 /+/, /-/

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

export
(/<</) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/<</) e1 e2 = MkBinaryExpression e1 MkShl e2

export
(/>>/) :
  Expression e1 =>
  Expression e2 =>
  e1 ->
  e2 ->
  BinaryExpression e1 e2
(/>>/) e1 e2 = MkBinaryExpression e1 MkShr e2

infixl 9 /*/, ///, /<</, />>/

export
(/:=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/:=/) ls rs = MkAssignmentStatement ls MkDefine rs Nothing

export
(/=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/=/) ls rs = MkAssignmentStatement ls MkAssign rs Nothing

export
(/+=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/+=/) ls rs = MkAssignmentStatement ls MkAddAssign rs Nothing

export
(/-=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/-=/) ls rs = MkAssignmentStatement ls MkSubAssign rs Nothing

infixl 7 /:=/, /=/, /+=/, /-=/

export
(/&/) :
  Expression e =>
  e ->
  UnaryExpression e
(/&/) e = MkUnaryExpression MkAnd e

export
star :
  Expression e =>
  e ->
  StarExpression e
star e = MkStarExpression e

prefix 4 /&/

