module Go.Token.Position

%default total

public export
FileName : Type
FileName = String

public export
Offset : Type
Offset = Nat

public export
Line : Type
Line = Nat

public export
Column : Type
Column = Nat

public export
record Position where
  constructor MkPosition
  filename : Maybe FileName
  offset : Offset
  line : Line
  column : Column

export
implementation Show Position where
  show p = case p.filename of
    Nothing => "-"
    Just f =>
      let c = if p.column == 0 then "" else ":\{show p.column}"
      in "\{f}:\{show p.line}\{c}"

export
implementation Eq Position where
  p1 == p2 = p1.filename == p2.filename
             && p1.offset == p2.offset
             && p1.line == p2.line
             && p1.column == p2.column

export
noPos : Position
noPos = MkPosition Nothing 0 0 0

export
isValid : Position -> Bool
isValid p = p /= noPos

export
(+) : Position -> Nat -> Position
(+) (MkPosition fn o ln cl) n = MkPosition fn o ln $ cl + n

