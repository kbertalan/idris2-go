module Go.AST

import Data.List
import Data.List.All
import Data.List.Last
import Data.List.Quantifiers
import Data.List1
import Go.Token
import Go.Token.Position

interface Node a where
  pos : a -> Maybe Position
  end : a -> Maybe Position

interface Node a => Expression a where
interface Node a => Statement a where
interface Node a => Declaration a where

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

