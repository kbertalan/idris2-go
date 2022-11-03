module Go.AST

import Data.List
import Data.List.Quantifiers
import Data.List1
import Go.Token

%default total

interface Node a where
interface Node a => Expression a where
interface Node a => Statement a where
interface Node a => Specification a where
interface Node a => Declaration a where

public export
record Comment where
  constructor MkComment
  text : String

public export
implementation Node Comment where

public export
data CommentGroup = MkCommentGroup (List1 Comment)

public export
implementation Node CommentGroup where

-- comment group text

-- isDirective

record Identifier
implementation Node Identifier
name : Identifier -> String

record BasicLiteral
implementation Node BasicLiteral

public export
record Field t where
  constructor MkField
  doc : Maybe CommentGroup
  names : List Identifier
  type : Maybe t
  tag : Maybe BasicLiteral
  comment : Maybe CommentGroup

public export
implementation Expression t => Node (Field t) where

public export
record FieldList (ts : List Type) where
  constructor MkFieldList
  list : All Field ts

public export 
implementation All Field ts => Node (FieldList ts) where

public export
record ArrayType l e where
  constructor MkArrayType
  length : l
  element : e

public export
implementation Expression l => Expression e => Node (ArrayType l e) where

public export
implementation Expression l => Expression e => Expression (ArrayType l e) where

public export
record StructType ts where
  constructor MkStructType
  fields : FieldList ts

public export
implementation Node (FieldList ts) => Node (StructType ts) where

public export
implementation Node (FieldList ts) => Expression (StructType ts) where

public export
record FunctionType ts ps rs where
  constructor MkFunctionType
  typeParams : FieldList ts
  params : FieldList ps
  results : FieldList rs

public export
implementation Node (FieldList ts) => Node (FieldList ps) => Node (FieldList rs) => Node (FunctionType ts ps rs) where

public export
implementation Node (FieldList ts) => Node (FieldList ps) => Node (FieldList rs) => Expression (FunctionType ts ps rs) where

public export
record InterfaceType ts where
  constructor MkInterfaceType
  methods : FieldList ts

public export
implementation Node (FieldList ts) => Node (InterfaceType ts) where

public export
implementation Node (FieldList ts) => Expression (InterfaceType ts) where

public export
record MapType k v where
  constructor MkMapType
  key : k
  value : v

public export
implementation Expression k => Expression v => Node (MapType k v) where

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
implementation Expression e => Node (ChanType e) where

public export
implementation Expression e => Expression (ChanType e) where

public export
record BadStatement where
  constructor MkBadStatement

public export
implementation Node BadStatement where

public export
implementation Statement BadStatement where

public export
record DeclarationStatement t where
  constructor MkDeclarationStatement
  declaration : t

public export
implementation Declaration t => Node (DeclarationStatement t) where

public export
implementation Declaration t => Statement (DeclarationStatement t) where

public export
record EmptyStatement where
  constructor MkEmptyStatement
  isImplicit : Bool

public export
implementation Node EmptyStatement where

public export
implementation Statement EmptyStatement where

public export
record LabeledStatement t where
  constructor MkLabeledStatement
  label : Identifier
  statement : t

public export
implementation Statement t => Node (LabeledStatement t) where

public export
implementation Statement t => Statement (LabeledStatement t) where

public export
record ExpressionStatement e where
  constructor MkExpressionStatement
  expression : e

public export
implementation Expression e => Node (ExpressionStatement e) where

public export
implementation Expression e => Statement (ExpressionStatement e) where

public export
record SendStatement c v where
  constructor MkSendStatement
  chan : c
  value : v

public export
implementation Expression c => Expression v => Node (SendStatement c v) where

public export
implementation Expression c => Expression v => Expression (SendStatement c v) where

public export
data IncOrDec : Operator -> Type where
  Inc : IncOrDec MkInc
  Dec : IncOrDec MkDec

public export
record IncDecStatement e o where
  constructor MkIncDecStatement
  expression : e
  token : IncOrDec o

public export
implementation Expression e => Node (IncDecStatement e o) where

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
implementation All Expression ls => All Expression rs => NonEmpty ls => NonEmpty rs => Node (AssignmentStatement ls rs) where

public export
implementation Node (AssignmentStatement ls rs) => Statement (AssignmentStatement ls rs) where

-- early definiton of Ellipsis
public export
record Ellipsis t where
  constructor MkEllipsis
  elementType : Maybe t

public export
implementation Expression t => Node (Ellipsis t) where

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
implementation Expression f => All Expression as => Expression e => Node (CallExpression f as e) where

public export
implementation Expression f => All Expression as => Expression e => Expression (CallExpression f as e) where

public export
record GoStatement f as e where
  constructor MkGoStatement
  call : CallExpression f as e


public export
implementation Node (CallExpression f as e) => Node (GoStatement f as e) where

public export
implementation Node (CallExpression f as e) => Statement (GoStatement f as e) where

public export
record DeferStatement f as e where
  constructor MkDeferStatement
  call : CallExpression f as e


public export
implementation Node (CallExpression f as e) => Node (DeferStatement f as e) where

public export
implementation Node (CallExpression f as e) => Statement (DeferStatement f as e) where

public export
record ReturnStatement rs where
  constructor MkReturnStatement
  results : HList rs

public export
implementation All Expression rs => Node (ReturnStatement rs) where

public export
implementation Node (ReturnStatement rs) => Statement (ReturnStatement rs) where

public export
data BranchStatementToken : Keyword -> Type where
  IsBreak : BranchStatementToken MkBreak
  IsContinue : BranchStatementToken MkContinue
  IsGoto : BranchStatementToken MkBreak
  IsFallthrough : BranchStatementToken MkFallthrough

public export
record BranchStatement kw where
  constructor MkBranchStatement
  token : BranchStatementToken kw
  label : Maybe Identifier

public export
implementation Node (BranchStatement kw) where

public export
implementation Statement (BranchStatement kw) where

public export
record BlockStatement sts where
  constructor MkBlockStatement
  statements : HList sts

public export
implementation All Statement sts => Node (BlockStatement sts) where

public export
implementation Node (BlockStatement sts) => Statement (BlockStatement sts) where

public export
record IfStatement i c sts e where
  constructor MkIfStatement
  init : Maybe i
  condition : c
  body : BlockStatement sts
  elseBranch : Maybe e

public export
implementation Statement i => Expression c => Statement (BlockStatement sts) => Statement e => Node (IfStatement i c sts e) where

public export
implementation Node (IfStatement i c sts e) => Statement (IfStatement i c sts e) where

public export
record CaseClause es sts where
  constructor MkCaseClause
  list : HList es
  body : HList sts

public export
implementation All Expression es => NonEmpty sts => All Statement sts => Node (CaseClause es sts) where

public export
data IsCaseClause : Type -> Type where
  ItIsCaseClause : IsCaseClause (CaseClause es sts)

public export
record SwitchStatement s e sts where
  constructor MkSwitchStatement
  init : Maybe s
  tag : Maybe e
  body : BlockStatement sts

public export
implementation Statement s => Expression e => Node (BlockStatement sts) => All IsCaseClause sts => Node (SwitchStatement e s sts) where

public export
implementation Node (SwitchStatement e s sts) => Statement (SwitchStatement e s sts) where

public export
record TypeSwitchStatement s a sts where
  constructor MkTypeSwitchStatement
  init : Maybe s
  assign : a
  body : BlockStatement sts

public export
implementation Statement s => Statement a => Node (BlockStatement sts) => All IsCaseClause sts => Node (TypeSwitchStatement e a sts) where

public export
implementation Node (TypeSwitchStatement e a sts) => Statement (TypeSwitchStatement e a sts) where

public export
data IsSendStatement : Type -> Type where
  ItIsSendStatement : IsSendStatement (SendStatement _ _)

public export
record CommClause s sts where
  constructor MkCommClause
  comm : Maybe s
  body : HList sts

public export
implementation IsSendStatement s => All Statement sts => Node (CommClause s sts) where

public export
record SelectStatement sts where
  constructor MkSelectStatement
  body : BlockStatement sts

public export
implementation Node (BlockStatement sts) => Node (SelectStatement sts) where

public export
implementation Node (SelectStatement sts) => Statement (SelectStatement sts) where

public export
record ForStatement i c p sts where
  constructor MkForStatement
  init : Maybe i
  condition : Maybe c
  post : Maybe p
  body : BlockStatement sts

public export
implementation Statement i => Expression c => Statement p => Node (BlockStatement sts) => Node (ForStatement i c p sts) where

public export
implementation Node (ForStatement i c p sts) => Statement (ForStatement i c p sts) where

public export
rangeWithKey : Type -> Type -> Type
rangeWithKey () _ = ()
rangeWithKey _ a = a

public export
data AssignOrDefine : Operator -> Type where
  ItIsAssign : AssignOrDefine MkAssign
  ItIsDefine : AssignOrDefine MkDefine

public export
record RangeStatement k v a e sts where
  constructor MkRangeStatement
  key : k
  value : rangeWithKey k v
  token : rangeWithKey k (AssignOrDefine a)
  expression : e
  body : BlockStatement sts

public export
implementation Node (BlockStatement sts) => Expression e => Node (RangeStatement () v a e sts) where

public export
implementation Expression k => Expression v => Expression e => Node (BlockStatement sts) => Node (RangeStatement k (Maybe v) a e sts) where

public export
implementation Node (RangeStatement k v a e sts) => Statement (RangeStatement k v a e sts) where

public export
implementation Node (BlockStatement sts) => Node (RangeStatement k v a e sts) where

--- Expressions

public export
record BadExpression where
  constructor MkBadExpression

public export
implementation Node BadExpression where

public export
implementation Expression BadExpression where

public export
record Identifier where
  constructor MkIdentifier
  name : String
  -- object : Maybe Object

public export
implementation Node Identifier where

public export
implementation Expression Identifier where

public export
record BasicLiteral where
  constructor MkBasicLiteral
  kind : Token.Literal
  value : String

public export
implementation Node BasicLiteral where

public export
implementation Expression BasicLiteral where

public export
record FunctionLiteral ts ps rs sts where
  constructor MkFunctionLiteral
  type : FunctionType ts ps rs
  body : BlockStatement sts

public export
implementation Expression (FunctionType ts ps rs) => Node (BlockStatement sts) => Node (FunctionLiteral ts ps rs sts) where

public export
implementation Expression (FunctionType ts ps rs) => Node (BlockStatement sts) => Expression (FunctionLiteral ts ps rs sts) where

public export
record CompositeLiteral t es where
  constructor MkCompositLiteral
  type : Maybe t
  expressions : HList es
  incomplete : Bool

public export
implementation Expression t => All Expression es => Node (CompositeLiteral t es) where

public export
implementation Expression t => All Expression es => Expression (CompositeLiteral t es) where

public export
record ParenExpression e where
  constructor MkParenExpression
  expression : e

public export
implementation Expression e => Node (ParenExpression e) where

public export
implementation Expression e => Expression (ParenExpression e) where

public export
record SelectorExpression e where
  constructor MkSelectorExpression
  expression : e
  selector : Identifier

public export
implementation Expression e => Node (SelectorExpression e) where

public export
implementation Expression e => Expression (SelectorExpression e) where

public export
record IndexExpression e i where
  constructor MkIndexExpression
  expression : e
  index : i

public export
implementation Expression e => Expression i => Node (IndexExpression e i) where

public export
implementation Expression e => Expression i => Expression (IndexExpression e i) where

public export
record IndexListExpression e is where
  constructor MkIndexListExpression
  expression : e
  indices : All Expression is

public export
implementation Expression e => All Expression is => Node (IndexListExpression e is) where

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
implementation Expression e => Expression l => Expression h => Expression m => Node (SliceExpression e l h m) where

public export
implementation Expression e => Expression l => Expression h => Expression m => Expression (SliceExpression e l h m) where

public export
record TypeAssertExpression e t where
  constructor MkTypeAssertExpression
  expression : e
  type : t

public export
implementation Expression e => Expression t => Node (TypeAssertExpression e t) where

public export
implementation Expression e => Expression t => Expression (TypeAssertExpression e t) where

public export
record StarExpression e where
  constructor MkStarExpresison
  expression : e

public export
implementation Expression e => Node (StarExpression e) where

public export
implementation Expression e => Expression (StarExpression e) where

public export
record UnaryExpression e where
  constructor MkUnaryExpression
  operator : Operator
  expression : e

public export
implementation Expression e => Node (UnaryExpression e) where

public export
implementation Expression e => Expression (UnaryExpression e) where

public export
record BinaryExpression x y where
  constructor MkBinaryExpression
  first : x
  operator : Operator
  last : y

public export
implementation Expression x => Expression y => Node (BinaryExpression x y) where

public export
implementation Expression x => Expression y => Expression (BinaryExpression x y) where

public export
record KeyValueExpression k v where
  constructor MkKeyValueExpression
  key : k
  value : v

public export
implementation Expression k => Expression v => Node (KeyValueExpression k v) where

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
implementation Node ImportSpec where

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
implementation Expression e => All Expression es => Node (ValueSpec e es) where


public export
implementation Node (ValueSpec e es) => Specification (ValueSpec e es) where

public export
record TypeSpec fs e where
  constructor MkTypeSpec
  doc : Maybe CommentGroup
  name : Identifier
  typeParams : Maybe $ FieldList fs
  type : e
  comment : Maybe CommentGroup

public export
implementation Expression e => Node (FieldList fs) => Node (TypeSpec fs e) where

public export
implementation Node (TypeSpec fs e) => Specification (TypeSpec fs e) where

-- declarations

public export
record BadDeclaration where
  constructor MkBadDeclaration

public export
implementation Node BadDeclaration where

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
record GenericDeclaration t xs where
  constructor MkGenericDeclaration
  doc : Maybe CommentGroup
  token : GenericDeclarationToken t
  specs : HList xs

public export
implementation NonEmpty xs => All Specification xs => Node (GenericDeclaration t xs) where

public export
implementation Node (GenericDeclaration t xs) => Declaration (GenericDeclaration t xs) where

public export
record FuncDeclaration rcs ts ps rs sts where
  constructor MkFuncDeclaration
  doc : Maybe CommentGroup
  reciever : FieldList rcs
  name : Identifier
  type : FunctionType ts ps rs
  body : BlockStatement sts

public export
implementation Node (FieldList rcs) => Expression (FunctionType ts ps rs) => Statement (BlockStatement sts) => Node (FuncDeclaration rcs ts ps rs sts) where

public export
implementation Node (FuncDeclaration rcs ts ps rs sts) => Declaration (FuncDeclaration rcs ts ps rs sts) where

public export
record File ds where
  constructor MkFile
  doc : Maybe CommentGroup
  name : Identifier
  decls : HList ds
  -- scope : Scope
  imports : List ImportSpec
  unresolved : List Identifier
  comments : List CommentGroup

public export
implementation All Declaration ds => Node (File ds) where

