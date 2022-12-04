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
implementation Statement (AssignmentStatement ls rs) => Commentable (AssignmentStatement ls rs) where
  setComments cg = { comment := Just cg }

export
implementation Statement (ExpressionStatement e) => Commentable (ExpressionStatement e) where
  setComments cg = { comment := Just cg }

export
implementation Specification (ValueSpec e es) => Commentable (ValueSpec e es) where
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
implementation Statement (AssignmentStatement ls rs) => Documentable (AssignmentStatement ls rs) where
  setDocs ds = { doc := Just ds }

export
implementation Specification (ValueSpec ls rs) => Documentable (ValueSpec ls rs) where
  setDocs ds = { doc := Just ds }

export
implementation Statement (ExpressionStatement e) => Documentable (ExpressionStatement e) where
  setDocs ds = { doc := Just ds }

export
implementation Statement (ReturnStatement rs) => Documentable (ReturnStatement rs) where
  setDocs ds = { doc := Just ds }

export
data Package = MkPackage String

export
package : String -> Package
package = MkPackage

export
id_ :
  String ->
  Identifier
id_ name = MkIdentifier name

export
file :
  { ds : List Type } ->
  { auto 0 ok : All Declaration ds } ->
  (name : String) ->
  Package ->
  List ImportSpec ->
  HList ds ->
  File ds
file name (MkPackage pkg) imports decls = MkFile Nothing name (id_ pkg) decls imports [] []

namespace Literal
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
  composit :
    GoType t =>
    All Expression es =>
    t ->
    HList es ->
    CompositLiteral t es
  composit t es = MkCompositLiteral (Just t) es

  export
  composit' :
    All Expression es =>
    HList es ->
    CompositLiteral BadType es
  composit' es = MkCompositLiteral Nothing es

  export
  funcL :
    All Statement sts =>
    FieldList ps ->
    FieldList rs ->
    HList sts ->
    FunctionLiteral [] ps rs sts
  funcL ps rs sts = MkFunctionLiteral (MkFunctionType [] ps rs) (MkBlockStatement sts)

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
field fs t = MkField Nothing (id_ <$> fs) (Just t) Nothing Nothing

export
field' :
  List String ->
  Field BadType
field' fs = MkField Nothing (id_ <$> fs) (Maybe BadType `the` Nothing) Nothing Nothing

namespace Type

  export
  tid :
    String ->
    String ->
    TypeIdentifier
  tid p n = MkTypeIdentifier (Just $ id_ p) (id_ n)

  export
  tid' :
    String ->
    TypeIdentifier
  tid' n = MkTypeIdentifier Nothing (id_ n)

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
  array l t = MkArrayType (Just l) t

  export
  array' :
    GoType t =>
    t ->
    ArrayType BadExpression t
  array' t = MkArrayType Nothing t

  export
  map_ :
    GoType k =>
    GoType v =>
    k ->
    v ->
    MapType k v
  map_ k v = MkMapType k v

  export
  func' :
    FieldList ps ->
    FieldList rs ->
    FunctionType [] ps rs
  func' ps rs = MkFunctionType [] ps rs

namespace Declaration
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
  type name typeParams t = MkTypeSpec Nothing (id_ name) typeParams t Nothing

  export
  consts :
    NonEmpty es =>
    All Specification es =>
    HList es ->
    GenericDeclaration MkConst es
  consts es = MkGenericDeclaration Nothing Const es

  export
  const_ :
    GoType t =>
    All Expression es =>
    (is : List Identifier) ->
    {auto 0 ok : NonEmpty is} ->
    t ->
    HList es ->
    ValueSpec t es
  const_ (i::is) t es = MkValueSpec Nothing (i:::is) (Just t) es Nothing

  export
  const' :
    All Expression es =>
    (is : List Identifier) ->
    {auto 0 ok : NonEmpty is} ->
    HList es ->
    ValueSpec BadType es
  const' (i::is) es = MkValueSpec Nothing (i:::is) Nothing es Nothing

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

namespace Statement

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
  for_ :
    Statement i =>
    Expression c =>
    Statement p =>
    All Statement sts =>
    i ->
    c ->
    p ->
    HList sts ->
    ForStatement i c p sts
  for_ i c p sts = MkForStatement (Just i) (Just c) (Just p) (MkBlockStatement sts)

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
  rangeKV :
    Expression r =>
    All Statement sts =>
    (key : String) ->
    (value : String) ->
    r ->
    HList sts ->
    KeyValueRangeStatement Identifier Identifier MkDefine r sts
  rangeKV k v r sts = MkKeyValueRangeStatement (id_ k) (id_ v) ItIsDefine r $ MkBlockStatement sts

  export
  rangeV :
    Expression r =>
    All Statement sts =>
    (value : String) ->
    r ->
    HList sts ->
    ValueRangeStatement Identifier MkDefine r sts
  rangeV v r sts = MkValueRangeStatement (id_ v) ItIsDefine r $ MkBlockStatement sts

  export
  range :
    Expression r =>
    All Statement sts =>
    r ->
    HList sts ->
    RangeStatement r sts
  range r sts = MkRangeStatement r $ MkBlockStatement sts

  export
  if_ :
    Expression c =>
    All Statement sts =>
    c ->
    HList sts ->
    IfStatement BadStatement c sts BadStatement
  if_ c sts = MkIfStatement Nothing c (MkBlockStatement sts) Nothing

  export
  ifS :
    Statement i =>
    Expression c =>
    All Statement sts =>
    i ->
    c ->
    HList sts ->
    IfStatement i c sts BadStatement
  ifS i c sts = MkIfStatement (Just i) c (MkBlockStatement sts) Nothing

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
  ifSE :
    Statement i =>
    Expression c =>
    All Statement sts =>
    Statement e =>
    i ->
    c ->
    HList sts ->
    e ->
    IfStatement i c sts e
  ifSE i c sts e = MkIfStatement (Just i) c (MkBlockStatement sts) (Just e)

  export
  switchS :
    Statement i =>
    Expression e =>
    All Statement sts =>
    All IsCaseClause sts =>
    i ->
    e ->
    HList sts ->
    SwitchStatement i e sts
  switchS i e sts = MkSwitchStatement (Just i) (Just e) (MkBlockStatement sts)

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
  case_ :
    All Expression es =>
    All Statement sts =>
    NonEmpty sts =>
    HList es ->
    HList sts ->
    CaseClause es sts
  case_ es sts = MkCaseClause es sts

  export
  default_ :
    All Statement sts =>
    NonEmpty sts =>
    HList sts ->
    CaseClause [] sts
  default_ sts = MkCaseClause [] sts

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
cast_ :
  GoType t =>
  Expression e =>
  t ->
  e ->
  CastExpression t e
cast_ = MkCastExpression

export
make :
  GoType t =>
  All Expression es =>
  t ->
  HList es ->
  MakeExpression t es
make t es = MkMakeExpression t es

export
index :
  Expression e =>
  Expression i =>
  e ->
  i ->
  IndexExpression e i
index e i = MkIndexExpression e i

export
slice :
  Expression e =>
  Expression l =>
  Expression h =>
  Expression m =>
  e ->
  l ->
  h ->
  m ->
  SliceExpression e l h m
slice e l h m = MkSliceExpression e (Just l) (Just h) (Just m)

export
sliceLH :
  Expression e =>
  Expression l =>
  Expression h =>
  e ->
  l ->
  h ->
  SliceExpression e l h BadExpression
sliceLH e l h = MkSliceExpression e (Just l) (Just h) Nothing

export
sliceL :
  Expression e =>
  Expression l =>
  e ->
  l ->
  SliceExpression e l BadExpression BadExpression
sliceL e l = MkSliceExpression e (Just l) Nothing Nothing

export
sliceH :
  Expression e =>
  Expression h =>
  e ->
  h ->
  SliceExpression e BadExpression h BadExpression
sliceH e h = MkSliceExpression e Nothing (Just h) Nothing

export
slice' :
  Expression e =>
  e ->
  SliceExpression e BadExpression BadExpression BadExpression
slice' e = MkSliceExpression e Nothing Nothing Nothing

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
(/./) e f = MkSelectorExpression e $ id_ f

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
(/:=/) ls rs = MkAssignmentStatement Nothing ls MkDefine rs Nothing

export
(/=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/=/) ls rs = MkAssignmentStatement Nothing ls MkAssign rs Nothing

export
(/+=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/+=/) ls rs = MkAssignmentStatement Nothing ls MkAddAssign rs Nothing

export
(/-=/) :
  All Expression ls =>
  All Expression rs =>
  NonEmpty ls =>
  NonEmpty rs =>
  HList ls ->
  HList rs ->
  AssignmentStatement ls rs
(/-=/) ls rs = MkAssignmentStatement Nothing ls MkSubAssign rs Nothing

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

