module Go.Token

import public Data.Fin
import Data.List
import public Go.Token.Position

public export
data Literal
  = MkIdentifier
  | MkInt
  | MkFloat
  | MkImag
  | MkChar
  | MkString

export
implementation Show Literal where
  show = \case
    MkIdentifier => "IDENT"
    MkInt => "INT"
    MkFloat => "FLOAT"
    MkImag => "IMAG"
    MkChar => "CHAR"
    MkString => "STRING"

public export
data Operator
  = MkAdd
  | MkSub
  | MkMul
  | MkQuo
  | MkRem
  | MkAnd
  | MkOr
  | MkXor
  | MkShl
  | MkShr
  | MkAndNot
  | MkAddAssign
  | MkSubAssign
  | MkMulAssign
  | MkQuoAssign
  | MkRemAssign
  | MkAndAssign
  | MkOrAssign
  | MkXorAssign
  | MkShlAssign
  | MkShrAssign
  | MkAndNotAssign
  | MkLogicalAnd
  | MkLogicalOr
  | MkArrow
  | MkInc
  | MkDec
  | MkEql
  | MkLess
  | MkGreater
  | MkAssign
  | MkNot
  | MkNotEql
  | MkLessThanOrEqual
  | MkGreaterThanOrEqual
  | MkDefine
  | MkEllipsis
  | MkLParen
  | MkLBracket
  | MkLBrace
  | MkComma
  | MkPeriod
  | MkRParen
  | MkRBracket
  | MkRBrace
  | MkSemicolon
  | MkColon

export
implementation Show Operator where
  show = \case
    MkAdd => "+"
    MkSub => "-"
    MkMul => "*"
    MkQuo => "/"
    MkRem => "%"
    MkAnd => "&"
    MkOr => "|"
    MkXor => "^"
    MkShl => "<<"
    MkShr => ">>"
    MkAndNot => "&^"
    MkAddAssign => "+="
    MkSubAssign => "-="
    MkMulAssign => "*="
    MkQuoAssign => "/="
    MkRemAssign => "%="
    MkAndAssign => "&="
    MkOrAssign => "|="
    MkXorAssign => "^="
    MkShlAssign => "<<="
    MkShrAssign => ">>="
    MkAndNotAssign => "&^="
    MkLogicalAnd => "&&"
    MkLogicalOr => "||"
    MkArrow => "<-"
    MkInc => "++"
    MkDec => "--"
    MkEql => "=="
    MkLess => "<"
    MkGreater => ">"
    MkAssign => "="
    MkNot => "!"
    MkNotEql => "!="
    MkLessThanOrEqual => "<="
    MkGreaterThanOrEqual => ">="
    MkDefine => ":="
    MkEllipsis => "..."
    MkLParen => "("
    MkLBracket => "["
    MkLBrace => "{"
    MkComma => ","
    MkPeriod => "."
    MkRParen => ")"
    MkRBracket => "]"
    MkRBrace => "}"
    MkSemicolon => ";"
    MkColon => ":"

public export
Precedence : Type
Precedence = Fin 8

export
precedence : Operator -> Precedence
precedence = \case
  MkLogicalOr => 1
  MkLogicalAnd => 2
  MkEql => 3
  MkNotEql => 3
  MkLess => 3
  MkLessThanOrEqual => 3
  MkGreater => 3
  MkGreaterThanOrEqual => 3
  MkAdd => 4
  MkSub => 4
  MkOr => 4
  MkXor => 4
  MkMul => 5
  MkQuo => 5
  MkRem => 5
  MkShl => 5
  MkShr => 5
  MkAnd => 5
  MkAndNot => 5
  _ => 7

public export
data Keyword
  = MkBreak
  | MkCase
  | MkChan
  | MkConst
  | MkContinue
  | MkDefault
  | MkDefer
  | MkElse
  | MkFallthrough
  | MkFor
  | MkFunc
  | MkGo
  | MkGoto
  | MkIf
  | MkImport
  | MkInterface
  | MkMap
  | MkPackage
  | MkRange
  | MkReturn
  | MkSelect
  | MkStruct
  | MkSwitch
  | MkType
  | MkVar

export
implementation Show Keyword where
  show = \case
    MkBreak => "break"
    MkCase => "case"
    MkChan => "chan"
    MkConst => "const"
    MkContinue => "continue"
    MkDefault => "default"
    MkDefer => "defer"
    MkElse => "else"
    MkFallthrough => "fallthrough"
    MkFor => "for"
    MkFunc => "func"
    MkGo => "go"
    MkGoto => "goto"
    MkIf => "if"
    MkImport => "import"
    MkInterface => "interface"
    MkMap => "map"
    MkPackage => "package"
    MkRange => "range"
    MkReturn => "return"
    MkSelect => "select"
    MkStruct => "struct"
    MkSwitch => "switch"
    MkType => "type"
    MkVar => "var"

public export
implementation Eq Keyword where
  MkBreak == MkBreak = True
  MkCase == MkCase = True
  MkChan == MkChan = True
  MkConst == MkConst = True
  MkContinue == MkContinue = True
  MkDefault == MkDefault = True
  MkDefer == MkDefer = True
  MkElse == MkElse = True
  MkFallthrough == MkFallthrough = True
  MkFor == MkFor = True
  MkFunc == MkFunc = True
  MkGo == MkGo = True
  MkGoto == MkGoto = True
  MkIf == MkIf = True
  MkImport == MkImport = True
  MkInterface == MkInterface = True
  MkMap == MkMap = True
  MkPackage == MkPackage = True
  MkRange == MkRange = True
  MkReturn == MkReturn = True
  MkSelect == MkSelect = True
  MkStruct == MkStruct = True
  MkSwitch == MkSwitch = True
  MkType == MkType = True
  MkVar == MkVar = True
  _ == _ = False

public export
keywords : List (String, Keyword)
keywords = map (\a => (show a, a))
  [ MkBreak
  , MkCase
  , MkChan
  , MkConst
  , MkContinue
  , MkDefault
  , MkDefer
  , MkElse
  , MkFallthrough
  , MkFor
  , MkFunc
  , MkGo
  , MkGoto
  , MkIf
  , MkImport
  , MkInterface
  , MkMap
  , MkPackage
  , MkRange
  , MkReturn
  , MkSelect
  , MkStruct
  , MkSwitch
  , MkType
  , MkVar
  ]

export
getKeyword : String -> Maybe Keyword
getKeyword str = List.lookup str keywords

public export
data Additional
  = MkTilde

export
implementation Show Additional where
  show = \case
    MkTilde => "~"

public export
data Token
  = MkIllegal
  | MkEOF
  | MkComment
  | MkLiteralToken Literal
  | MkOperatorToken Operator
  | MkKeywordToken Keyword
  | MkAdditionalToken Additional

export
implementation Show Token where
  show = \case
    MkIllegal => "ILLEGAL"
    MkEOF => "EOF"
    MkComment => "COMMENT"
    MkLiteralToken l => show l
    MkOperatorToken o => show o
    MkKeywordToken k => show k
    MkAdditionalToken a => show a

export
isLiteral : Token -> Bool
isLiteral = \case
  MkLiteralToken _ => True
  _ => False

export
isOperator : Token -> Bool
isOperator = \case
  MkOperatorToken _ => True
  MkAdditionalToken MkTilde => True
  _ => False

export
isKeyword : Token -> Bool
isKeyword = \case
  MkOperatorToken _ => True
  _ => False

export
lookup : String -> Token
lookup str =
  let Just keyword = getKeyword str
        | Nothing => MkLiteralToken MkIdentifier
  in MkKeywordToken keyword
