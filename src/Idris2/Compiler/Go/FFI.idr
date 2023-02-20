module Idris2.Compiler.Go.FFI

import Data.Maybe
import Data.String

import Libraries.Text.Lexer
import Libraries.Text.Parser

public export
data ForeignImport : Type where
  MkSupportImport : ForeignImport
  MkModuleImport : String -> String -> ForeignImport
  MkPathImport : String -> String -> ForeignImport

export
implementation Show ForeignImport where
  show = \case
    MkSupportImport => "MkSupportImport"
    MkModuleImport pkg path => "MkModuleImport \{pkg} \{path}"
    MkPathImport pkg path => "MkPathImport \{pkg} \{path}"

public export
data ParseFFIError
  = LexerFailed Int Int String
  | ParserMissingFunction
  | ParserMultipleFunctions
  | ParserFailed String

export
implementation Show ParseFFIError where
  show = \case
    LexerFailed l c u => "unrecognized token at line \{show l}, column \{show c}: \{u}"
    ParserMissingFunction => "could not find function definition"
    ParserMultipleFunctions => "there are multiple functions"
    ParserFailed u => "could not parse the FFI string: \{u}"

data TK
  = TKGo
  | TKImport
  | TKSpace
  | TKLParen
  | TKRParen
  | TKModule
  | TKSupport
  | TKPackage
  | TKPath

implementation Show TK where
  show = \case
    TKGo => "TKGo"
    TKImport => "TKImport"
    TKSpace => "TKSpace"
    TKLParen => "TKLParen"
    TKRParen => "TKRParen"
    TKModule => "TKModule"
    TKSupport => "TKSupport"
    TKPackage => "TKPackage"
    TKPath => "TKPath"

implementation Eq TK where
  TKGo == TKGo = True
  TKImport == TKImport = True
  TKSpace == TKSpace = True
  TKLParen == TKLParen = True
  TKRParen == TKRParen = True
  TKModule == TKModule = True
  TKSupport == TKSupport = True
  TKPackage == TKPackage = True
  TKPath == TKPath = True
  _ == _ = False

implementation TokenKind TK where
  TokType = \case
    TKGo => ()
    TKImport => ()
    TKSpace => ()
    TKLParen => ()
    TKRParen => ()
    TKModule => ()
    TKSupport => ()
    TKPackage => String
    TKPath => String

  tokValue tk s = case tk of
    TKGo => ()
    TKImport => ()
    TKSpace => ()
    TKLParen => ()
    TKRParen => ()
    TKModule => ()
    TKSupport => ()
    TKPackage => s
    TKPath => s

identifier : Lexer
identifier = some $ pred (\c => isAlphaNum c || c == '_')

tokenMapFFI : TokenMap $ Token TK
tokenMapFFI = toTokenMap $
  [ (exact "go:", TKGo)
  , (exact "import", TKImport)
  , (space, TKSpace)
  , (is '(', TKLParen)
  , (is ')', TKRParen)
  , (exact "%module%", TKModule)
  , (exact "%support%", TKSupport)
  , (identifier, TKPackage)
  , (stringLit, TKPath)
  ]

lexFFI : String -> Either (Int, Int, String) $ List (WithBounds $ Token TK)
lexFFI str =
  let (tokens, _, _, "") = lex tokenMapFFI str
          | (_, l, c, str) => Left (l, c, str)
      notSpace = \case
        TKSpace => False
        _ => True
  in Right $ filter (notSpace . kind . val) tokens

parserForeignImports : Grammar () (Token TK) True $ List ForeignImport
parserForeignImports = do
    match TKGo
    mImport <- optional $ match TKImport *> (singleImport <|> multipleImports)
    pure $ fromMaybe [] mImport
  where
    packageFromPath : String -> String
    packageFromPath path =
      let parts = split (=='/') path
      in last parts

    removeQuotes : String -> String
    removeQuotes str = pack $ goFirst $ unpack str
      where
        go : List Char -> List Char
        go ['"'] = []
        go [] = []
        go (x::xs) = x :: go xs

        goFirst : List Char -> List Char
        goFirst ('"'::xs) = go xs
        goFirst xs = go xs


    importModule : Grammar () (Token TK) True $ ForeignImport
    importModule = do
      mPackage <- optional $ match TKPackage
      match TKModule
      path <- removeQuotes <$> match TKPath
      pure $ MkModuleImport (fromMaybe (packageFromPath path) mPackage) path

    importSupport : Grammar () (Token TK) True $ ForeignImport
    importSupport = do
      match TKSupport
      pure MkSupportImport

    importPath : Grammar () (Token TK) True $ ForeignImport
    importPath = do
      mPackage <- optional $ match TKPackage
      path <- removeQuotes <$> match TKPath
      pure $ MkPathImport (fromMaybe (packageFromPath path) mPackage) path

    singleImport : Grammar () (Token TK) True $ List ForeignImport
    singleImport = do
      path <- removeQuotes <$> match TKPath
      pure [MkPathImport (packageFromPath path) path]

    multipleImports : Grammar () (Token TK) True $ List ForeignImport
    multipleImports = do
      match TKLParen
      res <- some $ importModule <|> importSupport <|> importPath
      match TKRParen
      pure $ forget res

showParsingError : ParsingError tok -> String
showParsingError (Error reason (Just bounds)) = "\{reason} at \{show $ startBounds bounds}"
showParsingError (Error reason Nothing) = reason

showToken : WithBounds (Token TK) -> String
showToken wb = "\{wb.val.text} - \{show wb.val.kind}"
showTokens : List (WithBounds $ Token TK) -> String
showTokens ts = joinBy "," $ map showToken ts

export
parseFFI : String -> Either ParseFFIError (List ForeignImport, String)
parseFFI str =
  let ls = lines $ trim str
      (imports, rest) = span (not . null . trim) ls
      Right tokens = lexFFI (unlines imports) | Left (l, c, unrecognized) => Left $ LexerFailed l c unrecognized
  in case parse parserForeignImports tokens of
        Right (_, is, []) => Right $ (is, trim $ unlines rest)
        Right (_, _, xs) => Left $ ParserFailed $ "unrecognized section after imports"
        Left errors => Left $ ParserFailed $ "\{show $ map showParsingError $ forget errors} in:\n\{str}\ntokens:\{showTokens tokens}"

