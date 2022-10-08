module Go.AST

import Data.HList
import Data.List
import Data.List.Quantifiers
import Data.List1
import Go.Token
import Go.Token.Position

%hide Data.List.Quantifiers.All.HList

interface Node a where
  pos : a -> Position
  end : a -> Position

interface Node a => Expression a where
interface Node a => Statement a where
interface Node a => Declaration a where

public export
record BadExpression where
  constructor MkBadExpression
  from, to: Position

public export
implementation Node BadExpression where
  pos = (.from)
  end = (.to)

public export
implementation Expression BadExpression where

public export
record Identifier where
  constructor MkIdentifier
  namePos : Position
  name : String
  -- object : Maybe Object

public export
implementation Node Identifier where
  pos = (.namePos)
  end i = i.namePos + length i.name

public export
implementation Expression Identifier where

public export
record Ellipsis t where
  constructor MkEllipsis
  pos : Position
  elementType : Maybe t

public export
implementation Expression t => Node (Ellipsis t) where
  pos = (.pos)
  end e with (e.elementType)
    _ | Just et = end et
    _ | Nothing = e.pos + 3 -- length of ...

public export
implementation Expression t => Expression (Ellipsis t) where

public export
record BasicLiteral where
  constructor MkBasicLiteral
  valuePos : Position
  kind : Token.Literal
  value : String

public export
implementation Node BasicLiteral where
  pos = (.valuePos)
  end b = b.valuePos + length b.value

public export
implementation Expression BasicLiteral where

public export
record Comment where
  constructor MkComment
  slash : Position
  text : String

public export
implementation Node Comment where
  pos c = c.slash
  end c = c.slash + length c.text

public export
data CommentGroup = MkCommentGroup (List1 Comment)

public export
implementation Node CommentGroup where
  pos (MkCommentGroup cs) = pos $ head cs
  end (MkCommentGroup cs) = end $ last cs

-- comment group text

-- isDirective

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
  pos f with (f.names)
   _ | n :: _ = pos n
   _ | [] with (f.type)
     _ | Just ty = pos ty
     _ | Nothing = noPos

  end f with (f.tag)
    _ | Just tag = end tag
    _ | Nothing with (f.type)
      _ | Just ty = end ty
      _ | Nothing with (f.names)
        _ | ns@(_::_) = end $ last ns
        _ | [] = noPos

public export
record FieldList (ts : List Type) where
  constructor MkFieldList
  opening : Position
  list : HList Field ts
  closing : Position

public export 
implementation Node (FieldList []) where
  pos fl = case isValid fl.opening of
    True => fl.opening
    False => noPos

  end fl = case isValid fl.closing of
    True => fl.closing
    False => noPos

public export
implementation Last l (t::ts) => Node (Field t) => Node (Field l) => Node (FieldList (t::ts)) where
  pos fl with (isValid fl.opening)
    _ | True = fl.opening
    _ | False = case fl.list of
        x::_ => pos x

  end fl with (isValid fl.closing)
    _ | True = fl.closing + 1
    _ | False = end {a = Field l} $ last fl.list

