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
implementation Expression (CompositLiteral t es) => Printer t => All Printer es => Printer (CompositLiteral t es) where
  print file cl = do
      case cl.type of
        Nothing => pure ()
        Just t => print file t
      case cl.expressions of
        x1::x2::x3::xs => do
          pPutStr "{"
          multiLine cl.expressions
          printNewLine
          printIndent
          pPutStr "}"
        _ => do
          pPutStr "{"
          singleLine cl.expressions
          pPutStr "}"
    where
      singleLine : {0 ts : List Type} -> {auto ps : All Printer ts} -> HList ts -> PrinterMonad io ()
      singleLine [] = pure ()
      singleLine {ps = [p]} [x] = print file x
      singleLine {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        singleLine xs

      inci : Indent
      inci = increaseIndent indent

      multiLine : {0 ts : List Type} -> {auto ps : All Printer ts} -> HList ts -> PrinterMonad io ()
      multiLine [] = pure ()
      multiLine {ps = [p]} [x] = do
        printNewLine
        printIndent {indent = inci}
        print {indent = inci} file x
        pPutStr ","
      multiLine {ps = (p::ps)} (x::xs) = do
        printNewLine
        printIndent {indent = inci}
        print {indent = inci} file x
        pPutStr ","
        multiLine xs

export
implementation Expression (FunctionLiteral ts ps rs sts) => Printer (FunctionType ts ps rs) => Printer (BlockStatement sts) => Printer (FunctionLiteral ts ps rs sts) where
  print file fl = do
    print file fl.type
    pPutStr " "
    print file fl.body

export
implementation Expression (CallExpression f as e) => Printer f => All Printer as => Printer (CallExpression f as e) where
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
implementation Expression (SelectorExpression e) => Printer e => Printer (SelectorExpression e) where
  print file se = do
    print file se.expression
    pPutStr "."
    print file se.selector

export
implementation Expression (IndexExpression e i) => Printer e => Printer i => Printer (IndexExpression e i) where
  print file ie = do
    print file ie.expression
    pPutStr "["
    print file ie.index
    pPutStr "]"

export
implementation Expression (SliceExpression e l h m) => Printer e => Printer l => Printer h => Printer m => Printer (SliceExpression e l h m) where
  print file se = do
    print file se.expression
    pPutStr "["
    case se.low of
      Nothing => pPutStr ":"
      Just l => do
        print file l
        pPutStr ":"
    case se.high of
      Nothing => pure ()
      Just h => print file h
    case se.max of
      Nothing => pure ()
      Just m => do
        pPutStr ":"
        print file m
    pPutStr "]"

export
implementation Expression (UnaryExpression e) => Printer e => Printer (UnaryExpression e) where
  print file ue = do
    pPutStr $ show ue.operator
    print file ue.expression

export
implementation Expression (StarExpression e) => Printer e => Printer (StarExpression e) where
  print file se = do
    pPutStr "*"
    print file se.expression

export
implementation Expression (BinaryExpression e1 e2) => Printer e1 => Printer e2 => Printer (BinaryExpression e1 e2) where
  print file bo = do
      print file bo.first
      pPutStr " "
      pPutStr $ show bo.operator
      pPutStr " "
      print file bo.last

export
implementation Expression (KeyValueExpression e1 e2) => Printer e1 => Printer e2 => Printer (KeyValueExpression e1 e2) where
  print file kve = do
    print file kve.key
    pPutStr ": "
    print file kve.value

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
implementation Specification (TypeSpec ts t) => Printer (FieldList ts) => Printer t => Printer (TypeSpec ts t) where
  print file ts = do
    print file ts.name
    case ts.typeParams of
      [] => pure ()
      xs => do
        pPutStr "["
        print file xs
        pPutStr "]"
    pPutStr " "
    print file ts.type

export
implementation Specification (ValueSpec t es) => Printer t => All Printer es => Printer (ValueSpec t es) where
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
      case vs.comment of
        Nothing => pure ()
        Just cg => do
          pPutStr " "
          printComments $ forget cg.comments
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
implementation Statement (ExpressionStatement e) => Printer e => Printer (ExpressionStatement e) where
  print file es = do
    case es.doc of
      Nothing => pure ()
      Just cg => do
        printComments $ forget cg.comments
        printNewLine
        printIndent
    print file es.expression
    case es.comment of
      Nothing => pure ()
      Just cg => do
        pPutStr " "
        printComments $ forget cg.comments

export
implementation Statement (DeclarationStatement d) => Printer d => Printer (DeclarationStatement d) where
  print file d = print file d.declaration

export
implementation Statement (BlockStatement sts) => All Printer sts => Printer (BlockStatement sts) where
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
implementation Statement (AssignmentStatement ls rs) => All Printer ls => All Printer rs => Printer (AssignmentStatement ls rs) where
  print file as = do
      case as.doc of
        Nothing => pure ()
        Just (MkCommentGroup cs) => do
          printComments $ forget cs
          printNewLine
          printIndent
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
implementation Statement (IncDecStatement e o) => Show (IncOrDec o) => Printer e => Printer (IncDecStatement e o) where
  print file ids = do
    print file ids.expression
    pPutStr $ show ids.token

export
implementation Statement (DeferStatement f as e) => Printer (CallExpression f as e) => Printer (DeferStatement f as e) where
  print file ds = do
    pPutStr "defer "
    print file ds.call

export
implementation Statement (ForStatement i c p sts) => Printer i => Printer c => Printer p => Printer (BlockStatement sts) => Printer (ForStatement i c p sts) where
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
implementation Statement (KeyValueRangeStatement k v a r sts) => Printer k => Printer v => Show (AssignOrDefine a) => Printer r => Printer (BlockStatement sts) => Printer (KeyValueRangeStatement k v a r sts) where
  print file rs = do
    pPutStr "for "
    print file rs.key
    pPutStr ", "
    print file rs.value
    pPutStr " \{show rs.token} range "
    print file rs.expression
    pPutStr " "
    print file rs.body

export
implementation Statement (ValueRangeStatement v a r sts) => Printer v => Show (AssignOrDefine a) => Printer r => Printer (BlockStatement sts) => Printer (ValueRangeStatement v a r sts) where
  print file rs = do
    pPutStr "for "
    print file rs.value
    pPutStr " \{show rs.token} range "
    print file rs.expression
    pPutStr " "
    print file rs.body

export
implementation Statement (RangeStatement r sts) => Printer r => Printer (BlockStatement sts) => Printer (RangeStatement r sts) where
  print file rs = do
    pPutStr "for range "
    print file rs.expression
    pPutStr " "
    print file rs.body

export
implementation Statement (IfStatement i c sts e) => Printer i => Printer c => Printer (BlockStatement sts) => Printer e => Printer (IfStatement i c sts e) where
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
implementation Statement (SwitchStatement i e sts) => Printer i => Printer e => All Printer sts => Printer (SwitchStatement i e sts) where
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
implementation Statement (CaseClause es sts) => All Printer es => All Printer sts => Printer (CaseClause es sts) where
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
implementation Statement (ReturnStatement rs) => All Printer rs => Printer (ReturnStatement rs) where
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
implementation GoType t => Printer t => Printer (Field t) where
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
  print file fl = many fl
    where
      many : { 0 ts : List Type } -> {auto ps : All Printer ts } -> FieldList ts -> PrinterMonad io ()
      many [] = pure ()
      many {ps = (p::ps)} [x] = print file x
      many {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        many xs {ps = ps}

-- Type

export
implementation Printer BadType where
  print file bt = pPutStr "/* Evaluating Bad Type */"

export
implementation Expression (StructType es) => All Printer es => Printer (FieldList es) => Printer (StructType es) where
  print file st = do
      pPutStr "struct {\n"
      many st.fields
      printIndent
      pPutStr "}" 
    where
      inci : Indent
      inci = increaseIndent indent

      many : { 0 ts : List Type } -> {auto ps : All Printer ts} -> FieldList ts -> PrinterMonad io ()
      many [] = pure ()
      many {ps = (p::ps)} (x::xs) = do
        printIndent {indent = inci}
        print {indent = inci} file x
        pPutStr "\n"
        many xs

export
implementation Expression (ArrayType l e) => Printer l => Printer e => Printer (ArrayType l e) where
  print file at = do
    pPutStr "["
    maybe (pure ()) (print file) at.length
    pPutStr "]"
    print file at.element

export
implementation GoType (MapType k v) => Printer k => Printer v => Printer (MapType k v) where
  print file mt = do
    pPutStr "map["
    print file mt.key
    pPutStr "]"
    print file mt.value

printReturnTypes : HasIO io => All Printer rs => File -> FieldList rs -> PrinterMonad io ()
printReturnTypes file fl = case fl of
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
implementation GoType (FunctionType ts ps rs) => All Printer ts => All Printer ps => All Printer rs => Printer (FunctionType ts ps rs) where
  print file ft = do
    pPutStr "func("
    print file ft.params
    pPutStr ")"
    printReturnTypes file ft.results

-- Declarations

export
implementation Declaration (FuncDeclaration rcs ts ps rs sts) => All Printer ps => All Printer rs => Printer (BlockStatement sts) => Printer (FuncDeclaration rcs ts ps rs sts) where
  print file fd = do
    pPutStr "func "
    pPutStr fd.name.name
    pPutStr "("
    print file fd.type.params
    pPutStr ")"
    printReturnTypes file fd.type.results
    pPutStr " "
    print file fd.body

export
implementation Declaration (GenericDeclaration k es) => All Printer es => Show (GenericDeclarationToken k) => Printer (GenericDeclaration k es) where
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
    printPackage f.packageName
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

export
printFile : HasIO io => All Declaration ds => Printer (Go.File ds) => (folder : String) -> Go.File ds -> io (Either PrintError ())
printFile folder f = do
  withFile "\{folder}/\{f.name}" WriteTruncate (pure . PrintFileError) $ \h =>
    runEitherT $ print h f

