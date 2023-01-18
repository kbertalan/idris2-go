module Prebuild

import Data.List
import Data.String
import System
import System.Directory
import System.Info
import System.File

template : String -> String
template content = """
module Idris2.Compiler.Go.Support.Gen

export
files : List (String, String)
files = [
\{content}
]
"""

separator : String
separator = if isWindows then "\\" else "/"

escape : String -> String
escape str = concat $ map escapeChar $ unpack str
  where
    escapeChar : Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar c = pack [c]

main : IO ()
main = do
  Right candidates <- listDir "support"
    | Left e => die $ show e

  let files = filter (isSuffixOf ".go") candidates

  let dir = joinBy separator ["src", "Idris2", "Compiler", "Go", "Support" ]
  result <- createDir dir
  case result of
    Left FileExists => pure ()
    Right () => pure ()
    Left e => die $ show e

  contents <- for files $ \f => do
    Right content <- readFile $ "support" ++ separator ++ f
      | Left e => die $ show e
    pure "(\"\{f}\", \"\"\"\n\{escape content}\"\"\")"

  let genFile = dir ++ separator ++ "Gen.idr"
  Right () <- writeFile genFile $ template $ joinBy ",\n" contents
    | Left e => die $ show e

  pure ()
