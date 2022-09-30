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

export
record Position where
  constructor MkPosition
  filename : Maybe FileName
  offset : Offset
  line : Line
  column : Column

export
valid : Position -> Bool
valid p = p.line > 0

export
implementation Show Position where
  show p = case p.filename of
    Nothing => "-"
    Just f =>
      let c = if p.column == 0 then "" else ":\{show p.column}"
      in "\{f}:\{show p.line}\{c}"

