module Go.AST

import Data.List
import Data.List.All
import Data.List.Last
import Data.List.Quantifiers
import Data.List1
import Go.Token
import Go.Token.Position

%default total

interface Node a where
  pos : a -> Maybe Position
  end : a -> Maybe Position

interface Node a => Expression a where
  constructor MkExpression
interface Node a => Statement a where
  constructor MkStatement
interface Node a => Specification a where
  constructor MkSpecification
interface Node a => Declaration a where
  constructor MkDeclaration

public export
record Comment where
  constructor MkComment
  slash : Maybe Position
  text : String

public export
implementation Node Comment where
  pos c = c.slash
  end c = map (+ length c.text) c.slash

public export
data CommentGroup = MkCommentGroup (List1 Comment)

public export
implementation Node CommentGroup where
  pos (MkCommentGroup cs) = pos $ head cs
  end (MkCommentGroup cs) = end $ last cs

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
  pos f = (pos =<< head' f.names)
          <|> (pos =<< f.type)
  end f = (end =<< f.tag)
          <|> (end =<< f.type)
          <|> (end =<< last' f.names)

public export
record FieldList (ts : List Type) where
  constructor MkFieldList
  opening : Maybe Position
  list : All Field ts
  closing : Maybe Position

public export 
implementation Node (FieldList []) where
  pos = (.opening)
  end = (.closing)

public export
implementation Last l (t::ts) => Node (Field t) => Node (Field l) => Node (FieldList (t::ts)) where
  pos fl = fl.opening
           <|> pos (head fl.list)

  end fl = map (+1) fl.closing
           <|> end {a = Field l} (last fl.list)

-- field list numFields

public export
record ArrayType l e where
  constructor MkArrayType
  lbracket : Maybe Position
  length : l
  element : e

public export
implementation Expression l => Expression e => Node (ArrayType l e) where
  pos at = at.lbracket
  end at = end at.element

public export
implementation Expression l => Expression e => Expression (ArrayType l e) where

public export
record StructType ts where
  constructor MkStructType
  struct : Maybe Position
  fields : FieldList ts
  incomplete : Bool

public export
implementation Node (FieldList ts) => Node (StructType ts) where
  pos st = st.struct
  end st = end st.fields

public export
implementation Node (FieldList ts) => Expression (StructType ts) where

public export
record FunctionType ts ps rs where
  constructor MkFunctionType
  func : Maybe Position
  typeParams : FieldList ts
  params : FieldList ps
  results : FieldList rs

public export
implementation Node (FieldList ts) => Node (FieldList ps) => Node (FieldList rs) => Node (FunctionType ts ps rs) where
  pos ft = ft.func <|> pos ft.params
  end ft = end ft.results <|> end ft.params

public export
implementation Node (FieldList ts) => Node (FieldList ps) => Node (FieldList rs) => Expression (FunctionType ts ps rs) where

public export
record InterfaceType ts where
  constructor MkInterfaceType
  interfacePos : Maybe Position
  methods : FieldList ts
  incomplete : Bool

public export
implementation Node (FieldList ts) => Node (InterfaceType ts) where
  pos it = it.interfacePos
  end it = end it.methods

public export
implementation Node (FieldList ts) => Expression (InterfaceType ts) where

public export
record MapType k v where
  constructor MkMapType
  mapPos : Maybe Position
  key : k
  value : v

public export
implementation Expression k => Expression v => Node (MapType k v) where
  pos mt = mt.mapPos
  end mt = end mt.value

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
  begin : Maybe Position
  arrowPos : Maybe Position
  direction : ChanDirection
  value : e

public export
implementation Expression e => Node (ChanType e) where
  pos ct = ct.begin
  end ct = end ct.value

public export
implementation Expression e => Expression (ChanType e) where

public export
record BadStatement where
  constructor MkBadStatement
  from, to : Maybe Position

public export
implementation Node BadStatement where
  pos = (.from)
  end = (.to)

public export
implementation Statement BadStatement where

public export
record DeclarationStatement t where
  constructor MkDeclarationStatement
  declaration : t

public export
implementation Declaration t => Node (DeclarationStatement t) where
  pos ds = pos ds.declaration
  end ds = end ds.declaration

public export
implementation Declaration t => Statement (DeclarationStatement t) where

public export
record EmptyStatement where
  constructor MkEmptyStatement
  semiColonPos : Maybe Position
  isImplicit : Bool

public export
implementation Node EmptyStatement where
  pos es = es.semiColonPos
  end es = map (\p => if es.isImplicit then p else p + 1) es.semiColonPos

public export
implementation Statement EmptyStatement where

public export
record LabeledStatement t where
  constructor MkLabeledStatement
  label : Identifier
  colonPos : Maybe Position
  statement : t

public export
implementation Statement t => Node (LabeledStatement t) where
  pos ls = pos ls.label
  end ls = end ls.statement

public export
implementation Statement t => Statement (LabeledStatement t) where

public export
record ExpressionStatement e where
  constructor MkExpressionStatement
  expression : e

public export
implementation Expression e => Node (ExpressionStatement e) where
  pos es = pos es.expression
  end es = end es.expression

public export
implementation Expression e => Statement (ExpressionStatement e) where

public export
record SendStatement c v where
  constructor MkSendStatement
  chan : c
  arrowPos : Maybe Position
  value : v

public export
implementation Expression c => Expression v => Node (SendStatement c v) where
  pos ss = pos ss.chan
  end ss = end ss.value

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
  tokenPos : Maybe Position
  token : IncOrDec o

public export
implementation Expression e => Node (IncDecStatement e o) where
  pos ids = pos ids.expression
  end ids = map (+2) ids.tokenPos

public export
implementation Expression e => Statement (IncDecStatement e o) where

public export
record AssignmentStatement ls rs where
  constructor MkAssignmentStatement
  left : HList ls
  tokenPos : Maybe Position
  right : HList rs

public export
implementation All Expression ls => All Expression rs => Expression l => NonEmpty rs => Last r rs => Expression r => Node (AssignmentStatement (l::ls) rs) where
  pos as = pos $ head as.left
  end as = end {a = r} $ last as.right

public export
implementation Node (AssignmentStatement ls rs) => Statement (AssignmentStatement ls rs) where

-- early definiton of Ellipsis
public export
record Ellipsis t where
  constructor MkEllipsis
  pos : Maybe Position
  elementType : Maybe t

public export
implementation Expression t => Node (Ellipsis t) where
  pos = (.pos)
  end e = case e.elementType of
            Just et => end et
            Nothing => map (+3) e.pos -- length of ...

public export
implementation Expression t => Expression (Ellipsis t) where

-- early definition of CallExpression
public export
record CallExpression f as e where
  constructor MkCallExpression
  function : f
  lparen : Maybe Position
  args : HList as
  ellipsis : Maybe $ Ellipsis e
  rparen : Maybe Position

public export
implementation Expression f => All Expression as => Expression e => Node (CallExpression f as e) where
  pos ce = pos ce.function
  end ce = map (+1) ce.rparen

public export
implementation Expression f => All Expression as => Expression e => Expression (CallExpression f as e) where

public export
record GoStatement f as e where
  constructor MkGoStatement
  goPos : Maybe Position
  call : CallExpression f as e


public export
implementation Node (CallExpression f as e) => Node (GoStatement f as e) where
  pos gs = gs.goPos
  end gs = end gs.call

public export
implementation Node (CallExpression f as e) => Statement (GoStatement f as e) where

public export
record DeferStatement f as e where
  constructor MkDeferStatement
  deferPos : Maybe Position
  call : CallExpression f as e


public export
implementation Node (CallExpression f as e) => Node (DeferStatement f as e) where
  pos ds = ds.deferPos
  end ds = end ds.call

public export
implementation Node (CallExpression f as e) => Statement (DeferStatement f as e) where

public export
record ReturnStatement rs where
  constructor MkReturnStatement
  returnPos : Maybe Position
  results : HList rs

public export
implementation Node (ReturnStatement []) where
  pos rs = rs.returnPos
  end rs = map (+6) rs.returnPos -- length of 'return'

public export
implementation Last l (r::rs) => Expression l => All Expression (r::rs) => Node (ReturnStatement (r::rs)) where
  pos rs = rs.returnPos
  end rs = end {a = l} $ last rs.results

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
  tokenPos : Maybe Position
  token : BranchStatementToken kw
  label : Maybe Identifier

public export
implementation Node (BranchStatement kw) where
  pos bs = bs.tokenPos
  end bs = (end =<< bs.label) <|> [| bs.tokenPos + (length . name <$> bs.label) |]

public export
implementation Statement (BranchStatement kw) where

public export
record BlockStatement sts where
  constructor MkBlockStatement
  lbrace : Maybe Position
  statements : HList sts
  rbrace : Maybe Position

public export
implementation All Statement sts => Node (BlockStatement sts) where
  pos bs = bs.lbrace
  end bs = map (+1) bs.rbrace

-- public export
-- implementation Node (BlockStatement []) where
--   pos bs = bs.lbrace
--   end bs = map (+1) bs.rbrace <|> map (+1) bs.lbrace
--
-- public export
-- implementation NonEmpty sts => Last l sts => Statement l => All Statement sts => Node (BlockStatement sts) where
--   pos bs = bs.lbrace
--   end bs = map (+1) bs.rbrace <|> (end {a = l} $ last bs.statements)

public export
implementation Node (BlockStatement sts) => Statement (BlockStatement sts) where

public export
record IfStatement i c sts e where
  constructor MkIfStatement
  ifPos : Maybe Position
  init : Maybe i
  condition : c
  body : BlockStatement sts
  elseBranch : Maybe e

public export
implementation Statement i => Expression c => Statement (BlockStatement sts) => Statement e => Node (IfStatement i c sts e) where
  pos is = is.ifPos
  end is = (end =<< is.elseBranch) <|> (end $ is.body)

public export
implementation Node (IfStatement i c sts e) => Statement (IfStatement i c sts e) where

public export
record CaseClause es sts where
  constructor MkCaseClause
  tokenPos : Maybe Position
  list : HList es
  colonPos : Maybe Position
  body : HList sts

public export
implementation All Expression es => Node (CaseClause es []) where
  pos cc = cc.tokenPos
  end cc = map (+1) cc.colonPos

public export
implementation All Expression es => NonEmpty sts => Last l sts => Statement l => All Statement sts => Node (CaseClause es sts) where
  pos cc = cc.tokenPos
  end cc = end {a = l} $ last cc.body

public export
data IsCaseClause : Type -> Type where
  ItIsCaseClause : IsCaseClause (CaseClause es sts)

public export
record SwitchStatement s e sts where
  constructor MkSwitchStatement
  switchPos : Maybe Position
  init : Maybe s
  tag : Maybe e
  body : BlockStatement sts

public export
implementation Statement s => Expression e => Node (BlockStatement sts) => All IsCaseClause sts => Node (SwitchStatement e s sts) where
  pos ss = ss.switchPos
  end ss = end ss.body

public export
implementation Node (SwitchStatement e s sts) => Statement (SwitchStatement e s sts) where

public export
record TypeSwitchStatement s a sts where
  constructor MkTypeSwitchStatement
  switchPos : Maybe Position
  init : Maybe s
  assign : a
  body : BlockStatement sts

public export
implementation Statement s => Statement a => Node (BlockStatement sts) => All IsCaseClause sts => Node (TypeSwitchStatement e a sts) where
  pos tss = tss.switchPos
  end tss = end tss.body

public export
implementation Node (TypeSwitchStatement e a sts) => Statement (TypeSwitchStatement e a sts) where

public export
data IsSendStatement : Type -> Type where
  ItIsSendStatement : IsSendStatement (SendStatement _ _)

public export
record CommClause s sts where
  constructor MkCommClause
  tokenPos : Maybe Position
  comm : Maybe s
  colonPos : Maybe Position
  body : HList sts

public export
implementation IsSendStatement s => Node (CommClause s []) where
  pos cc = cc.tokenPos
  end cc = map (+1) cc.colonPos

public export
implementation IsSendStatement s => NonEmpty sts => Last l sts => Statement l => All Statement sts => Node (CommClause s sts) where
  pos cc = cc.tokenPos
  end cc = end { a = l } $ last cc.body

public export
record SelectStatement sts where
  constructor MkSelectStatement
  selectPos : Maybe Position
  body : BlockStatement sts

public export
implementation Node (BlockStatement sts) => Node (SelectStatement sts) where
  pos ss = ss.selectPos
  end ss = end ss.body

public export
implementation Node (SelectStatement sts) => Statement (SelectStatement sts) where

public export
record ForStatement i c p sts where
  constructor MkForStatement
  forPos : Maybe Position
  init : Maybe i
  condition : Maybe c
  post : Maybe p
  body : BlockStatement sts

public export
implementation Statement i => Expression c => Statement p => Node (BlockStatement sts) => Node (ForStatement i c p sts) where
  pos fs = fs.forPos
  end fs = end fs.body

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
  forPos : Maybe Position
  key : k
  value : rangeWithKey k v
  tokenPos : rangeWithKey k $ Maybe Position
  token : rangeWithKey k (AssignOrDefine a)
  expression : e
  body : BlockStatement sts

public export
implementation Node (BlockStatement sts) => Expression e => Node (RangeStatement () v a e sts) where
  pos rs = rs.forPos
  end rs = end rs.body

public export
implementation Expression k => Expression v => Expression e => Node (BlockStatement sts) => Node (RangeStatement k (Maybe v) a e sts) where
  pos rs = rs.forPos
  end rs = end rs.body

public export
implementation Node (RangeStatement k v a e sts) => Statement (RangeStatement k v a e sts) where

public export
implementation Node (BlockStatement sts) => Node (RangeStatement k v a e sts) where
  pos rs = rs.forPos
  end rs = end rs.body

--- Expressions

public export
record BadExpression where
  constructor MkBadExpression
  from, to: Maybe Position

public export
implementation Node BadExpression where
  pos = (.from)
  end = (.to)

public export
implementation Expression BadExpression where

public export
record Identifier where
  constructor MkIdentifier
  namePos : Maybe Position
  name : String
  -- object : Maybe Object

public export
implementation Node Identifier where
  pos = (.namePos)
  end i = map (+ length i.name) i.namePos

public export
implementation Expression Identifier where

public export
record BasicLiteral where
  constructor MkBasicLiteral
  valuePos : Maybe Position
  kind : Token.Literal
  value : String

public export
implementation Node BasicLiteral where
  pos = (.valuePos)
  end b = map (+ length b.value) b.valuePos

public export
implementation Expression BasicLiteral where

public export
record FunctionLiteral ts ps rs sts where
  constructor MkFunctionLiteral
  type : FunctionType ts ps rs
  body : BlockStatement sts

public export
implementation Expression (FunctionType ts ps rs) => Node (BlockStatement sts) => Node (FunctionLiteral ts ps rs sts) where
  pos fl = pos fl.type
  end fl = end fl.body

public export
implementation Expression (FunctionType ts ps rs) => Node (BlockStatement sts) => Expression (FunctionLiteral ts ps rs sts) where

public export
record CompositeLiteral t es where
  constructor MkCompositLiteral
  type : Maybe t
  lbrace : Maybe Position
  expressions : HList es
  rbrace : Maybe Position
  incomplete : Bool

public export
implementation Expression t => All Expression es => Node (CompositeLiteral t es) where
  pos cl = (pos =<< cl.type) <|> cl.lbrace
  end cl = map (+1) cl.rbrace

public export
implementation Expression t => All Expression es => Expression (CompositeLiteral t es) where

public export
record ParenExpression e where
  constructor MkParenExpression
  lparen : Maybe Position
  expression : e
  rparen : Maybe Position

public export
implementation Expression e => Node (ParenExpression e) where
  pos = (.lparen)
  end pe = map (+1) pe.lparen

public export
implementation Expression e => Expression (ParenExpression e) where

public export
record SelectorExpression e where
  constructor MkSelectorExpression
  expression : e
  selector : Identifier

public export
implementation Expression e => Node (SelectorExpression e) where
  pos se = pos se.expression
  end se = end se.selector

public export
implementation Expression e => Expression (SelectorExpression e) where

public export
record IndexExpression e i where
  constructor MkIndexExpression
  expression : e
  lbracket : Maybe Position
  index : i
  rbracket : Maybe Position

public export
implementation Expression e => Expression i => Node (IndexExpression e i) where
  pos ie = pos ie.expression
  end ie = map (+1) ie.rbracket

public export
implementation Expression e => Expression i => Expression (IndexExpression e i) where

public export
record IndexListExpression e is where
  constructor MkIndexListExpression
  expression : e
  lbracket : Maybe Position
  indices : All Expression is
  rbracket : Maybe Position

public export
implementation Expression e => All Expression is => Node (IndexListExpression e is) where
  pos ile = pos ile.expression
  end ile = map (+1) ile.rbracket

public export
implementation Expression e => All Expression is => Expression (IndexListExpression e is) where

public export
record SliceExpression e l h m where
  constructor MkSliceExpression
  expression : e
  lbracket : Maybe Position
  low : Maybe l
  high : Maybe h
  max : Maybe m
  rbracket : Maybe Position

public export
implementation Expression e => Expression l => Expression h => Expression m => Node (SliceExpression e l h m) where
  pos se = pos se.expression
  end se = map (+1) se.rbracket

public export
implementation Expression e => Expression l => Expression h => Expression m => Expression (SliceExpression e l h m) where

public export
record TypeAssertExpression e t where
  constructor MkTypeAssertExpression
  expression : e
  lparen : Maybe Position
  type : t
  rparen : Maybe Position

public export
implementation Expression e => Expression t => Node (TypeAssertExpression e t) where
  pos tae = pos tae.expression
  end tae = map (+1) tae.rparen

public export
implementation Expression e => Expression t => Expression (TypeAssertExpression e t) where

public export
record StarExpression e where
  constructor MkStarExpresison
  star : Maybe Position
  expression : e

public export
implementation Expression e => Node (StarExpression e) where
  pos se = se.star
  end se = end se.expression

public export
implementation Expression e => Expression (StarExpression e) where

public export
record UnaryExpression e where
  constructor MkUnaryExpression
  operatorPos : Maybe Position
  operator : Operator
  expression : e

public export
implementation Expression e => Node (UnaryExpression e) where
  pos ue = ue.operatorPos
  end ue = end ue.expression

public export
implementation Expression e => Expression (UnaryExpression e) where

public export
record BinaryExpression x y where
  constructor MkBinaryExpression
  first : x
  operatorPos : Maybe Position
  operator : Operator
  last : y

public export
implementation Expression x => Expression y => Node (BinaryExpression x y) where
  pos be = pos be.first
  end be = end be.last

public export
implementation Expression x => Expression y => Expression (BinaryExpression x y) where

public export
record KeyValueExpression k v where
  constructor MkKeyValueExpression
  key : k
  colon : Maybe Position
  value : v

public export
implementation Expression k => Expression v => Node (KeyValueExpression k v) where
  pos kve = pos kve.key
  end kve = end kve.value

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
  endPos : Maybe Position

public export
implementation Node ImportSpec where
  pos is = (pos =<< is.name) <|> pos is.path
  end is = is.endPos <|> pos is.path

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
implementation Expression e => Node (ValueSpec e []) where
  pos vs = pos $ head vs.names
  end vs = end =<< vs.type

public export
implementation Expression e => NonEmpty es => Last l es => Expression l => All Expression es => Node (ValueSpec e es) where
  pos vs = pos $ head vs.names
  end vs = end {a = l} (last vs.values) <|> (end =<< vs.type) <|> end (last vs.names)

public export
implementation Node (ValueSpec e es) => Specification (ValueSpec e es) where

public export
record TypeSpec fs e where
  constructor MkTypeSpec
  doc : Maybe CommentGroup
  name : Identifier
  typeParams : Maybe $ FieldList fs
  assignPos : Maybe Position
  type : e
  comment : Maybe CommentGroup

public export
implementation Expression e => Node (FieldList fs) => Node (TypeSpec fs e) where
  pos ts = pos ts.name
  end ts = end ts.type

public export
implementation Node (TypeSpec fs e) => Specification (TypeSpec fs e) where

-- declarations

public export
record BadDeclaration where
  constructor MkBadDeclaration
  from, to : Maybe Position

public export
implementation Node BadDeclaration where
  pos bd = bd.from
  end bd = bd.to

public export
implementation Declaration BadDeclaration where

public export
data GenericDeclarationToken : Keyword -> Type where
  Import : GenericDeclarationToken MkImport
  Const : GenericDeclarationToken MkConst
  Type' : GenericDeclarationToken MkType
  Var : GenericDeclarationToken MkVar

public export
record GenericDeclaration t xs where
  constructor MkGenericDeclaration
  doc : Maybe CommentGroup
  tokenPos : Maybe Position
  token : GenericDeclarationToken t
  lparen : Maybe Position
  specs : HList xs
  rparen : Maybe Position

public export
implementation NonEmpty xs => All Specification xs => Last l xs => Specification l => Node (GenericDeclaration t xs) where
  pos gd = gd.tokenPos
  end gd = gd.rparen <|> end {a = l} (last gd.specs)

public export
implementation Node (GenericDeclaration t xs) => Specification (GenericDeclaration t xs) where

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
  pos fd = pos fd.type
  end fd = end fd.body <|> end fd.type

public export
implementation Node (FuncDeclaration rcs ts ps rs sts) => Declaration (FuncDeclaration rcs ts ps rs sts) where

public export
record File ds where
  constructor MkFile
  doc : Maybe CommentGroup
  package : Maybe Position
  name : Identifier
  decls : HList ds
  fileStart, fileEnd : Maybe Position
  -- scope : Scope
  imports : List ImportSpec
  unresolved : List Identifier
  comments : List CommentGroup

public export
implementation Node (File []) where
  pos f = f.package
  end f = end f.name

public export
implementation NonEmpty ds => All Declaration ds => Last l ds => Declaration l => Node (File ds) where
  pos f = f.package
  end f = end {a = l} (last f.decls) <|> end f.name

