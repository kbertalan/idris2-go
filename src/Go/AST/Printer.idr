module Go.AST.Printer

import Control.Monad.Either
import Data.List
import Data.List.All
import Data.List1
import Data.List.Quantifiers
import Go.AST as Go
import Go.Token
import System.File

%default total
%hide Prelude.print

public export
data PrintError
  = PrintFileError FileError

export
implementation Show PrintError where
  show = \case
    PrintFileError e => show e

public export
PrinterMonad : (Type -> Type) -> Type -> Type
PrinterMonad io a = EitherT PrintError io a

public export
data Indent = MkIndent Nat

export
zeroIndent : Indent
zeroIndent = MkIndent 0

export
increaseIndent : Indent -> Indent
increaseIndent (MkIndent i) = MkIndent (i+1)

public export
interface Printer a where
  constructor MkPrinter
  print : HasIO io => File -> {auto indent : Indent} -> a -> PrinterMonad io ()

pPutStr : HasIO io => {auto file : File} -> String -> PrinterMonad io ()
pPutStr {file} str = MkEitherT $ fPutStr file str >>= \case
  Left e => pure $ Left $ PrintFileError e
  Right a => pure $ Right a

printNewLine : HasIO io => {auto file : File} -> PrinterMonad io ()
printNewLine = pPutStr "\n"

printIndent : HasIO io => {auto file : File} -> {auto indent : Indent} -> PrinterMonad io ()
printIndent {indent = (MkIndent i)} =
  let str = concat $ replicate i "\t"
  in pPutStr str

printComments : HasIO io => {auto file : File} -> {auto indent : Indent} -> List Comment -> PrinterMonad io ()
printComments [] = pure ()
printComments [x] = do
    pPutStr "//"
    pPutStr x.text
printComments (x::xs) = do
    pPutStr "//"
    pPutStr x.text
    printNewLine
    printIndent
    printComments xs

-- Expression

export
implementation Printer BadExpression where
  print file be = pPutStr "/* Evaluated BadExpression */"

export
implementation Printer Identifier where
  print file i = pPutStr i.name

export
implementation Printer BasicLiteral where
  print file bl =
    let value = case bl.kind of
                  MkString => "\"\{bl.value}\""
                  _ => bl.value
    in pPutStr value

export
implementation Expression f => All Expression as => Expression e => Printer f => All Printer as => Printer (CallExpression f as e) where
  print file ce = do
      print file ce.function
      pPutStr "("
      many ce.args
      pPutStr ")"
    where
      many : { 0 ts : List Type } -> {auto ps : All Printer ts} -> HList ts -> PrinterMonad io ()
      many [] = pure ()
      many {ps = [p]} [x] = print file x
      many {ps = [p1,p2]} [x,y] = do
        print file x
        pPutStr ", "
        print file y
      many {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        many xs

export
implementation Expression e => Printer e => Printer (SelectorExpression e) where
  print file se = do
    print file se.expression
    pPutStr "."
    print file se.selector

export
implementation Expression e => Printer e => Printer (UnaryExpression e) where
  print file ue = do
    pPutStr $ show ue.operator
    print file ue.expression

export
implementation Expression e1 => Expression e2 => Printer e1 => Printer e2 => Printer (BinaryExpression e1 e2) where
  print file bo = do
      print file bo.first
      pPutStr " "
      pPutStr $ show bo.operator
      pPutStr " "
      print file bo.last

-- Spec

export
implementation Printer ImportSpec where
  print file is = case is.name of
    Nothing => print file is.path
    Just i => do
      print file i
      pPutStr " "
      print file is.path

export
implementation Expression t => All Expression es => Printer t => All Printer es => Printer (ValueSpec t es) where
  print file vs = do
      case vs.doc of
        Nothing => pure ()
        Just (MkCommentGroup cs) => do
          printComments $ forget cs
          printNewLine
          printIndent
      printNames $ List1.forget vs.names
      case vs.type of
        Nothing => pure ()
        Just t => do
          pPutStr " "
          print file t
      when (not $ null vs.values) $ do
        pPutStr " = "
        printValues vs.values
    where
      printNames : List Identifier -> PrinterMonad io ()
      printNames [] = pure ()
      printNames [x] = print file x
      printNames (x::xs) = do
          print file x
          pPutStr ", "
          printNames xs

      printValues : { 0 ts : List Type } -> {auto ps : All Printer ts } -> HList ts -> PrinterMonad io ()
      printValues [] = pure ()
      printValues {ps = [p]} [x] = print file x
      printValues {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        printValues xs

-- Statements

export
implementation Printer BadStatement where
  print file be = pPutStr "/* Evaluated BadStatement */"

export
implementation Expression e => Printer e => Printer (ExpressionStatement e) where
  print file es = do
    case es.doc of
      Nothing => pure ()
      Just cg => do
        printComments $ forget cg.comments
        printNewLine
        printIndent
    print file es.expression

export
implementation Declaration d => Printer d => Printer (DeclarationStatement d) where
  print file d = print file d.declaration

export
implementation All Statement sts => All Printer sts => Printer (BlockStatement sts) where
  print file bs @{stss} @{ps} = do
      pPutStr "{\n"
      many bs.statements {ps}
      printIndent
      pPutStr "}"
    where
      inci : Indent
      inci = increaseIndent indent

      many : { 0 sts : List Type } -> { ps : All Printer sts } -> HList sts -> PrinterMonad io ()
      many [] = pure ()
      many {ps = [p]} [x] = do
        printIndent {indent = inci}
        print {indent = inci} file x
        printNewLine
      many {ps = (p::ps)} (x::xs) = do
        printIndent {indent = inci}
        print {indent = inci} file x
        printNewLine
        many xs {ps}

export
implementation All Expression ls => All Expression rs => NonEmpty ls => NonEmpty rs => All Printer ls => All Printer rs => Printer (AssignmentStatement ls rs) where
  print file as = do
      many as.left
      pPutStr " "
      pPutStr $ show as.token
      pPutStr " "
      many as.right
      case as.comment of
        Nothing => pure ()
        Just (MkCommentGroup cs) => do
          pPutStr " "
          printComments $ forget cs
          when (1 < length cs) printNewLine
    where
      many : {0 es : List Type} -> {auto ps : All Printer es} -> HList es -> PrinterMonad io ()
      many [] = pure ()
      many {ps = [p]} [x] = do
        print file x
      many {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        many xs

export
implementation Expression e => Show (IncOrDec o) => Printer e => Printer (IncDecStatement e o) where
  print file ids = do
    print file ids.expression
    pPutStr $ show ids.token

export
implementation Statement i => Expression c => Statement p => All Statement sts => Printer i => Printer c => Printer p => Printer (BlockStatement sts) => Printer (ForStatement i c p sts) where
  print file fs = do
    pPutStr "for"
    case (fs.init, fs.condition, fs.post) of
      (Nothing, Nothing, Nothing) => pure ()
      (Nothing, Just c, Nothing) => do
        pPutStr " "
        print file c
      _ => do
        pPutStr " "
        maybe (pure ()) (print file) fs.init
        pPutStr "; "
        maybe (pure ()) (print file) fs.condition
        pPutStr "; "
        maybe (pure ()) (print file) fs.post

    pPutStr " "
    print file fs.body

export
implementation Statement i => Expression c => All Statement sts => Printer i => Printer c => Printer (BlockStatement sts) => Printer e => Printer (IfStatement i c sts e) where
  print file is = do
    pPutStr "if "
    case is.init of
      Nothing => pure ()
      Just i => do
        print file i
        pPutStr "; "
    print file is.condition
    pPutStr " "
    print file is.body
    case is.elseBranch of
      Nothing => pure ()
      Just e => do
        pPutStr " else "
        print file e

export
implementation Statement i => Expression e => All Statement sts => All IsCaseClause sts => Printer i => Printer e => All Printer sts => Printer (SwitchStatement i e sts) where
  print file ss = do
      pPutStr "switch "
      case ss.init of
        Nothing => pure ()
        Just i => do
          print file i
          pPutStr "; "
      case ss.tag of
        Nothing => pure ()
        Just e => do
          print file e
          pPutStr " "
      pPutStr "{"
      printBody ss.body.statements
      printNewLine
      printIndent
      pPutStr "}"
    where
      printBody : { 0 sts : List Type } -> {auto ps : All Printer sts } -> HList sts -> PrinterMonad io ()
      printBody [] = pure ()
      printBody {ps = [p]} [x] = do
        printNewLine
        printIndent
        print file x
      printBody {ps = (p::ps)} (x::xs) = do
        printNewLine
        printIndent
        print file x
        printBody xs

export
implementation All Expression es => NonEmpty sts => All Statement sts => All Printer es => All Printer sts => Printer (CaseClause es sts) where
  print file cc = do
      case cc.list of
        [] => pPutStr "default"
        _ => pPutStr "case "
      printList cc.list
      pPutStr ":"
      printBody cc.body
    where
      printList : {0 ts : List Type} -> {auto ps : All Printer ts} -> HList ts -> PrinterMonad io ()
      printList [] = pure ()
      printList {ps = [p]} [x] = print file x
      printList {ps = p::ps} (x::xs) = do
        print file x
        pPutStr ", "
        printList xs

      inci : Indent
      inci = increaseIndent indent

      printBody : {0 ts : List Type} -> {auto ps : All Printer ts} -> HList ts -> PrinterMonad io ()
      printBody [] = pure ()
      printBody {ps = [p]} [x] = do
        printNewLine
        printIndent {indent = inci}
        print {indent = inci} file x
      printBody {ps = p::ps} (x::xs) = do
        printNewLine
        printIndent {indent = inci}
        print {indent = inci} file x
        printList xs

export
implementation All Expression rs => All Printer rs => Printer (ReturnStatement rs) where
  print file rs = do
      case rs.doc of
        Nothing => pure ()
        Just ds => do
          printComments $ forget ds.comments
          printNewLine
          printIndent
      pPutStr "return"
      many rs.results
    where
      many : {0 es : List Type} -> {auto ps : All Printer es} -> HList es -> PrinterMonad io ()
      many [] = pure ()
      many {ps = [p]} [x] = do
        pPutStr " "
        print file x
      many {ps = (p::ps)} (x::xs) = do
        pPutStr " "
        print file x
        pPutStr ","
        many xs

-- Fields

export
implementation Printer t => Printer (Field t) where
  print file f = do
    printNames f.names
    case f.type of
      Nothing => pure ()
      Just t => do
        when (not $ null f.names) $ pPutStr " "
        print file t
    where
      printNames : List Identifier -> PrinterMonad io ()
      printNames [] = pure ()
      printNames [x] = print file x
      printNames (x::xs) = do
        print file x
        pPutStr ", "
        printNames xs

export
implementation All Printer ts => Printer (FieldList ts) where
  print file fl @{ps} = many fl.list {ps = ps}
    where
      many : { 0 ts : List Type } -> { ps : All Printer ts } -> All Field ts -> PrinterMonad io ()
      many [] = pure ()
      many {ps = (p::ps)} [x] = print file x
      many {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        many xs {ps = ps}

-- Declarations

export
implementation All Statement sts => All Printer ps => All Printer rs => Printer (BlockStatement sts) => All Field rs => Printer (FuncDeclaration rcs ts ps rs sts) where
  print file fd = do
    pPutStr "func "
    pPutStr fd.name.name
    pPutStr "("
    print file fd.type.params
    pPutStr ")"
    printReturnTypes fd.type.results
    pPutStr " "
    print file fd.body

    where
      printReturnTypes : FieldList rs -> PrinterMonad io ()
      printReturnTypes fl = case fl.list of
        [] => print file fl
        [x] => do
          let parens = 2 <= length x.names
          pPutStr " "
          when parens $ pPutStr "("
          print file fl
          when parens $ pPutStr ")"
        xs => do
          pPutStr " ("
          print file fl
          pPutStr ")"

export
implementation NonEmpty es => All Specification es => All Printer es => Show (GenericDeclarationToken k) => Printer (GenericDeclaration k es) where
  print file gd = do
      let multiple = hasMany gd.specs
          inci = if multiple then increaseIndent indent else indent
      pPutStr $ show gd.token
      pPutStr " "
      when multiple $ do
        pPutStr "(\n"
        printIndent {indent=inci}
      many inci gd.specs
      when multiple $ pPutStr "\n)"
    where
      hasMany : {0 ts : List Type} -> HList ts -> Bool
      hasMany [] = False
      hasMany [_] = False
      hasMany _ = True

      many : {0 ts : List Type} -> {auto ps : All Printer ts} -> Indent -> HList ts -> PrinterMonad io ()
      many _ [] = pure ()
      many {ps = [p]} i [x] = print {indent=i} file x
      many {ps = (p::ps)} i (x::xs) = do
        print {indent=i} file x
        printNewLine
        printIndent {indent=i}
        many i xs

-- File

export
implementation All Declaration ds => All Printer ds => Printer (Go.File ds) where
  print file f = do
    printIndent
    printPackage f.name
    printNewLine
    printNewLine
    printIndent
    printImports f.imports
    printNewLine
    printNewLine
    printIndent
    printDecls f.decls
    printNewLine

    where
      printDecls : { 0 ds : List Type } -> { auto ps : All Printer ds } -> HList ds -> PrinterMonad io ()
      printDecls {ds = []} Nil = pure ()
      printDecls {ds = [x]} {ps = [p]} [d] = print file d
      printDecls {ds = x::xs} {ps = p::ps} (d::ds) = do
        print file d @{p}
        printNewLine
        printNewLine
        printIndent
        printDecls ds

      printPackage : Identifier -> PrinterMonad io ()
      printPackage i = pPutStr "package \{i.name}"

      printImports : List ImportSpec -> PrinterMonad io ()
      printImports = \case
        [] => pure ()
        [x] => do
          pPutStr "import "
          print file x
        xs => do
          pPutStr "import (\n"
          ignore $ for xs $ \spec => do
            printIndent {indent = increaseIndent indent}
            print file spec
            printNewLine
          pPutStr ")"

