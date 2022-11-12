module Go.AST

import Data.List
import Data.List.Quantifiers
import Data.List1
import Go.Token

%default total

interface Expression a where
interface Statement a where
interface Specification a where
interface Declaration a where
interface GoType a where

public export
record Comment where
  constructor MkComment
  text : String

public export
record CommentGroup where
  constructor MkCommentGroup
  comments : List1 Comment

record Identifier
record BasicLiteral

public export
record Field t {auto 0 tt : GoType t} where
  constructor MkField
  doc : Maybe CommentGroup
  names : List Identifier
  type : Maybe t
  tag : Maybe BasicLiteral
  comment : Maybe CommentGroup

public export
data FieldList : (ts : List Type) -> Type where
  Nil : FieldList []
  (::) : {auto tt : GoType t} -> Field t -> FieldList ts -> FieldList (t::ts)

public export
record ArrayType l e where
  constructor MkArrayType
  length : l
  element : e

public export
implementation Expression l => Expression e => Expression (ArrayType l e) where

public export
record StructType ts where
  constructor MkStructType
  fields : FieldList ts

public export
implementation Expression (StructType ts) where

public export
implementation GoType (StructType ts) where

public export
record FunctionType ts ps rs where
  constructor MkFunctionType
  typeParams : FieldList ts
  params : FieldList ps
  results : FieldList rs

public export
implementation Expression (FunctionType ts ps rs) where

public export
implementation GoType (FunctionType ts ps rs) where

public export
record InterfaceType ts where
  constructor MkInterfaceType
  methods : FieldList ts

public export
implementation Expression (InterfaceType ts) where

public export
implementation GoType (InterfaceType ts) where

public export
record MapType k v where
  constructor MkMapType
  key : k
  value : v

public export
implementation Expression k => Expression v => Expression (MapType k v) where

public export
data ChanDirection
  = Send
  | Receive
  | Both

public export
record ChanType e where
  constructor MkChanType
  direction : ChanDirection
  value : e

public export
implementation Expression e => Expression (ChanType e) where

public export
record BadStatement where
  constructor MkBadStatement

public export
implementation Statement BadStatement where

public export
record DeclarationStatement t where
  constructor MkDeclarationStatement
  declaration : t

public export
implementation Declaration t => Statement (DeclarationStatement t) where

public export
record EmptyStatement where
  constructor MkEmptyStatement
  isImplicit : Bool

public export
implementation Statement EmptyStatement where

public export
record LabeledStatement t where
  constructor MkLabeledStatement
  label : Identifier
  statement : t

public export
implementation Statement t => Statement (LabeledStatement t) where

public export
record ExpressionStatement e where
  constructor MkExpressionStatement
  doc : Maybe CommentGroup
  expression : e
  comment : Maybe CommentGroup

public export
implementation Expression e => Statement (ExpressionStatement e) where

public export
record SendStatement c v where
  constructor MkSendStatement
  chan : c
  value : v

public export
implementation Expression c => Expression v => Expression (SendStatement c v) where

public export
data IncOrDec : Operator -> Type where
  Inc : IncOrDec MkInc
  Dec : IncOrDec MkDec

public export
implementation Show (IncOrDec MkInc) where
  show _ = show MkInc

public export
implementation Show (IncOrDec MkDec) where
  show _ = show MkDec

public export
record IncDecStatement e o where
  constructor MkIncDecStatement
  expression : e
  token : IncOrDec o

public export
implementation Expression e => Statement (IncDecStatement e o) where

public export
record AssignmentStatement ls rs where
  constructor MkAssignmentStatement
  left : HList ls
  token : Operator
  right : HList rs
  comment : Maybe CommentGroup

public export
implementation All Expression ls => All Expression rs => NonEmpty ls => NonEmpty rs => Statement (AssignmentStatement ls rs) where

-- early definiton of Ellipsis
public export
record Ellipsis t where
  constructor MkEllipsis
  elementType : Maybe t

public export
implementation Expression t => Expression (Ellipsis t) where

-- early definition of CallExpression
public export
record CallExpression f as e where
  constructor MkCallExpression
  function : f
  args : HList as
  ellipsis : Maybe $ Ellipsis e

public export
implementation Expression f => All Expression as => Expression e => Expression (CallExpression f as e) where

public export
record GoStatement f as e where
  constructor MkGoStatement
  call : CallExpression f as e

public export
implementation Expression f => All Expression as => Expression e => Statement (GoStatement f as e) where

public export
record DeferStatement f as e where
  constructor MkDeferStatement
  call : CallExpression f as e


public export
implementation Expression f => All Expression as => Expression e => Statement (DeferStatement f as e) where

public export
record ReturnStatement rs where
  constructor MkReturnStatement
  doc : Maybe CommentGroup
  results : HList rs

public export
implementation All Expression rs => Statement (ReturnStatement rs) where

public export
data BranchStatementToken : Keyword -> Type where
  IsBreak : BranchStatementToken MkBreak
  IsContinue : BranchStatementToken MkContinue
  IsGoto : BranchStatementToken MkBreak
  IsFallthrough : BranchStatementToken MkFallthrough

public export
record BranchStatement (kw : Keyword) where
  constructor MkBranchStatement
  token : BranchStatementToken kw
  label : Maybe Identifier

public export
implementation Statement (BranchStatement kw) where

public export
record BlockStatement sts where
  constructor MkBlockStatement
  statements : HList sts

public export
implementation All Statement sts => Statement (BlockStatement sts) where

public export
record IfStatement i c sts e where
  constructor MkIfStatement
  init : Maybe i
  condition : c
  body : BlockStatement sts
  elseBranch : Maybe e

public export
implementation Statement i => Expression c => All Statement sts => Statement (IfStatement i c sts e) where

public export
record CaseClause es sts where
  constructor MkCaseClause
  list : HList es
  body : HList sts

public export
implementation All Expression es => NonEmpty sts => All Statement sts => Statement (CaseClause es sts) where

public export
data IsCaseClause : Type -> Type where
  ItIsCaseClause : All Expression es => NonEmpty sts => All Statement sts => IsCaseClause (CaseClause es sts)

public export
record SwitchStatement i e sts where
  constructor MkSwitchStatement
  init : Maybe i
  tag : Maybe e
  body : BlockStatement sts

public export
implementation Statement i => Expression e => All Statement sts => All IsCaseClause sts => Statement (SwitchStatement i e sts) where

public export
record TypeSwitchStatement i a sts where
  constructor MkTypeSwitchStatement
  init : Maybe i
  assign : a
  body : BlockStatement sts

public export
implementation Statement i => Statement a => All Statement sts => Statement (TypeSwitchStatement i a sts) where

public export
data IsSendStatement : Type -> Type where
  ItIsSendStatement : Expression c => Expression v => IsSendStatement (SendStatement c v)

public export
record CommClause s sts where
  constructor MkCommClause
  comm : Maybe s
  body : HList sts

public export
implementation IsSendStatement s => All Statement sts => Statement (CommClause s sts) where

public export
record SelectStatement sts where
  constructor MkSelectStatement
  body : BlockStatement sts

public export
implementation All Statement sts => Statement (SelectStatement sts) where

public export
record ForStatement i c p sts where
  constructor MkForStatement
  init : Maybe i
  condition : Maybe c
  post : Maybe p
  body : BlockStatement sts

public export
implementation Statement i => Expression c => Statement p => All Statement sts => Statement (ForStatement i c p sts) where

public export
data AssignOrDefine : Operator -> Type where
  ItIsAssign : AssignOrDefine MkAssign
  ItIsDefine : AssignOrDefine MkDefine

public export
record KeyValueRangeStatement k v a e sts where
  constructor MkKeyValueRangeStatement
  key : k
  value : v
  token : AssignOrDefine a
  expression : e
  body : BlockStatement sts

public export
implementation Expression k => Expression v => AssignOrDefine a => Expression e => All Statement sts => Statement (KeyValueRangeStatement k v a e sts) where

public export
record ValueRangeStatement v a e sts where
  constructor MkValueRangeStatement
  value : v
  token : AssignOrDefine a
  expression : e
  body : BlockStatement sts

public export
implementation Expression v => AssignOrDefine a => Expression e => All Statement sts => Statement (ValueRangeStatement v a e sts) where

public export
record RangeStatement e sts where
  constructor MkRangeStatement
  expression : e
  body : BlockStatement sts

public export
implementation Expression e => All Statement sts => Statement (RangeStatement e sts) where

--- Expressions

public export
record BadExpression where
  constructor MkBadExpression

public export
implementation Expression BadExpression where

public export
record Identifier where
  constructor MkIdentifier
  name : String
  -- object : Maybe Object

public export
implementation Expression Identifier where

public export
implementation GoType Identifier where

public export
record BasicLiteral where
  constructor MkBasicLiteral
  kind : Token.Literal
  value : String

public export
implementation Expression BasicLiteral where

public export
record FunctionLiteral ts ps rs sts where
  constructor MkFunctionLiteral
  type : FunctionType ts ps rs
  body : BlockStatement sts

public export
implementation All Statement sts => Expression (FunctionType ts ps rs) => Expression (FunctionLiteral ts ps rs sts) where

public export
record CompositLiteral t es where
  constructor MkCompositLiteral
  type : Maybe t
  expressions : HList es

public export
implementation Expression t => All Expression es => Expression (CompositLiteral t es) where

public export
record ParenExpression e where
  constructor MkParenExpression
  expression : e

public export
implementation Expression e => Expression (ParenExpression e) where

public export
record SelectorExpression e where
  constructor MkSelectorExpression
  expression : e
  selector : Identifier

public export
implementation Expression e => Expression (SelectorExpression e) where

public export
record IndexExpression e i where
  constructor MkIndexExpression
  expression : e
  index : i

public export
implementation Expression e => Expression i => Expression (IndexExpression e i) where

public export
record IndexListExpression e is where
  constructor MkIndexListExpression
  expression : e
  indices : HList is

public export
implementation Expression e => All Expression is => Expression (IndexListExpression e is) where

public export
record SliceExpression e l h m where
  constructor MkSliceExpression
  expression : e
  low : Maybe l
  high : Maybe h
  max : Maybe m

public export
implementation Expression e => Expression l => Expression h => Expression m => Expression (SliceExpression e l h m) where

public export
record TypeAssertExpression e t where
  constructor MkTypeAssertExpression
  expression : e
  type : t

public export
implementation Expression e => Expression t => Expression (TypeAssertExpression e t) where

public export
record StarExpression e where
  constructor MkStarExpression
  expression : e

public export
implementation Expression e => Expression (StarExpression e) where

public export
record UnaryExpression e where
  constructor MkUnaryExpression
  operator : Operator
  expression : e

public export
implementation Expression e => Expression (UnaryExpression e) where

public export
record BinaryExpression x y where
  constructor MkBinaryExpression
  first : x
  operator : Operator
  last : y

public export
implementation Expression x => Expression y => Expression (BinaryExpression x y) where

public export
record KeyValueExpression k v where
  constructor MkKeyValueExpression
  key : k
  value : v

public export
implementation Expression k => Expression v => Expression (KeyValueExpression k v) where

--- Specifications

public export
record ImportSpec where
  constructor MkImportSpec
  doc : Maybe CommentGroup
  name : Maybe Identifier
  path : BasicLiteral
  comment : Maybe CommentGroup

public export
implementation Specification ImportSpec where

public export
record ValueSpec e es where
  constructor MkValueSpec
  doc : Maybe CommentGroup
  names : List1 Identifier
  type : Maybe e
  values : HList es
  comment : Maybe CommentGroup

public export
implementation Expression e => All Expression es => Specification (ValueSpec e es) where

public export
record TypeSpec fs e where
  constructor MkTypeSpec
  doc : Maybe CommentGroup
  name : Identifier
  typeParams : FieldList fs
  type : e
  comment : Maybe CommentGroup

public export
implementation Expression e => Specification (TypeSpec fs e) where

-- declarations

public export
record BadDeclaration where
  constructor MkBadDeclaration

public export
implementation Declaration BadDeclaration where

public export
data GenericDeclarationToken : Keyword -> Type where
  Import : GenericDeclarationToken MkImport
  Const : GenericDeclarationToken MkConst
  Type' : GenericDeclarationToken MkType
  Var : GenericDeclarationToken MkVar

public export
implementation Show (GenericDeclarationToken MkImport) where
  show _ = show MkImport

public export
implementation Show (GenericDeclarationToken MkConst) where
  show _ = show MkConst

public export
implementation Show (GenericDeclarationToken MkType) where
  show _ = show MkType

public export
implementation Show (GenericDeclarationToken MkVar) where
  show _ = show MkVar

public export
record GenericDeclaration (t : Keyword) xs where
  constructor MkGenericDeclaration
  doc : Maybe CommentGroup
  token : GenericDeclarationToken t
  specs : HList xs

public export
implementation NonEmpty xs => All Specification xs => Declaration (GenericDeclaration t xs) where

public export
record FuncDeclaration rcs ts ps rs sts where
  constructor MkFuncDeclaration
  doc : Maybe CommentGroup
  reciever : FieldList rcs
  name : Identifier
  type : FunctionType ts ps rs
  body : BlockStatement sts

public export
implementation All Statement sts => Declaration (FuncDeclaration rcs ts ps rs sts) where

public export
record File ds {auto 0 dsd : All Declaration ds} where
  constructor MkFile
  doc : Maybe CommentGroup
  name : Identifier
  decls : HList ds
  -- scope : Scope
  imports : List ImportSpec
  unresolved : List Identifier
  comments : List CommentGroup

public export
data BadType = MkBadType

public export
implementation GoType BadType where

