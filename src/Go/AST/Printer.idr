module Go.AST.Printer

import Control.Monad.Either
import Data.List
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
implementation Printer ImportSpec where
  print file is = case is.name of
    Nothing => print file is.path
    Just i => do
      print file i
      pPutStr " "
      print file is.path

export
implementation Printer f => All Printer as => Printer (CallExpression f as e) where
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
implementation Printer e => Printer (ExpressionStatement e) where
  print file es = do
    printIndent
    print file es.expression

export
implementation All Printer sts => Printer (BlockStatement sts) where
  print file bs @{ps} = do
      pPutStr " {\n"
      many bs.statements {ps}
      pPutStr "}\n"
    where
      many : { 0 sts : List Type } -> { ps : All Printer sts } -> HList sts -> PrinterMonad io ()
      many [] = printNewLine
      many {ps = [p]} [x] = do
        print file {indent = increaseIndent indent} x
        printNewLine
      many {ps = (p::ps)} (x::xs) = do
        print file {indent = increaseIndent indent} x
        printNewLine
        many xs {ps}

export
implementation Printer t => Printer (Field t) where
  print file f = do
    printNames f.names
    case f.type of
      Nothing => pure ()
      Just t => print file t
    where
      printNames : List Identifier -> PrinterMonad io ()
      printNames [] = pure ()
      printNames [x] = print file x
      printNames [x,y] = do
        print file x
        pPutStr ", "
        print file y
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
      many {ps = [p1,p2]} [x,y] = do
        print file x
        pPutStr ", "
        print file y
      many {ps = (p::ps)} (x::xs) = do
        print file x
        pPutStr ", "
        many xs {ps = ps}


export
implementation All Printer ps => All Printer rs => Printer (BlockStatement sts) => All Field rs => Printer (FuncDeclaration rcs ts ps rs sts) where
  print file fd = do
    pPutStr "func "
    pPutStr fd.name.name
    pPutStr "("
    print file fd.type.params
    pPutStr ")"
    printReturnTypes fd.type.results
    print file fd.body

    where
      printReturnTypes : FieldList rs -> PrinterMonad io ()
      printReturnTypes fl = case fl.list of
        [] => print file fl
        [x] => print file fl
        xs => do
          pPutStr "("
          print file fl
          pPutStr ")"

export
implementation All Printer ds => Printer (Go.File ds) where
  print file f = do
    printIndent
    printPackage f.name
    printNewLine
    printIndent
    printImports f.imports
    printDecls f.decls

    where
      printDecls : { 0 ds : List Type } -> { auto ps : All Printer ds } -> HList ds -> PrinterMonad io ()
      printDecls {ds = []} Nil = pure ()
      printDecls {ds = x::xs} {ps = p::ps} (d::ds) = do
        printNewLine
        print file d @{p}
        printIndent
        printDecls ds

      printPackage : Identifier -> PrinterMonad io ()
      printPackage i = pPutStr "package \{i.name}\n"

      printImports : List ImportSpec -> PrinterMonad io ()
      printImports = \case
        [] => pure ()
        [x] => do
          pPutStr "import "
          print file x
          printNewLine
        xs => do
          pPutStr "import (\n"
          ignore $ for xs $ \spec => do
            printIndent {indent = increaseIndent indent}
            print file spec
            printNewLine
          pPutStr ")\n"

export
implementation Printer e1 => Printer e2 => Printer (BinaryExpression e1 e2) where
  print file bo = do
    print file bo.first
    pPutStr $ show bo.operator
    print file bo.last

