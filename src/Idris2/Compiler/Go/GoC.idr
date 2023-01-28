module Idris2.Compiler.Go.GoC

import Data.List1
import Data.String
import System
import System.File

import Core.Core
import Core.Context
import Libraries.Utils.Path

%default total

export
getArg : String -> String -> Maybe String
getArg directive key =
  let (k,v) = break (== '=') directive
  in
    if (trim k) == key
      then Just $ trim $ substr 1 (fromInteger $ cast (length v) - 1) v
      else Nothing

export
getFirstArg : List String -> String -> Maybe String
getFirstArg [] _ = Nothing
getFirstArg (x::xs) key =
  let Just arg = getArg x key
        | Nothing => getFirstArg xs key
  in Just arg

export
findGo : IO String
findGo = do
  Nothing <- getEnv "IDRIS2_GO"
    | Just goc => pure goc
  pure "go"

initGoMod :
  (directives : List String) ->
  (outDir : String) ->
  (moduleName : String) ->
  (go : String) ->
  Core (Maybe String)
initGoMod ds outDir moduleName go = do
  let mGoMod = getFirstArg ds "go.mod"
  case mGoMod of
    Just goMod => do
      Right () <- coreLift $ copyFile goMod $ outDir </> "go.mod"
        | Left (err, written) => pure $ Just "go.mod copy failed"
      Right () <- coreLift $ copyFile (goSumFrom goMod) $ outDir </> "go.sum"
        | Left (err, written) => pure Nothing
      pure Nothing
    Nothing => do
      0 <- coreLift $ system "cd \{outDir} && \{go} mod init \{moduleName} 2> /dev/null"
        | _ => pure $ Just "go.mod init failed"
      pure Nothing
  where
    goSumFrom : String -> String
    goSumFrom goMod =
      let parts = reverse $ forget $ split (=='.') goMod
          sum = case parts of
                  "mod" :: xs => "sum" :: xs
                  xs => xs
      in joinBy "." $ reverse sum

export
compileProgram :
  (directives : List String) ->
  (moduleName : String) ->
  (outDir : String) ->
  (outFile : String) ->
  Core (Maybe String)
compileProgram ds moduleName outDir outFile = do
  go <- coreLift findGo
  Nothing <- initGoMod ds outDir moduleName go
    | Just e => throw $ Fatal $ GenericMsg emptyFC $ show e

  0 <- coreLift $ system "cd \{outDir} && \{go} build -o \{outFile}"
    | _ => pure Nothing

  pure $ Just outFile

