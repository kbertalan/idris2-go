module Main

import Compiler.Common
import Compiler.CompileExpr

import Core.Context

import Idris.Driver
import Idris.Syntax

import Idris2.Compiler.Go

import Libraries.Utils.Path

import System
import System.Clock

export
compileExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (tmpDir : String) ->
  (outDir : String) ->
  ClosedTerm ->
  (outFile : String) ->
  Core (Maybe String)
compileExpr c s tmpDir outDir tm outFile = do
  cdata <- getCompileDataWith ["go"] False Cases tm
  let defs = namedDefs cdata
  compileGo outDir outFile defs $ forget cdata.mainExpr

export
executeExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (execDir : String) ->
  ClosedTerm -> 
  Core ()
executeExpr c s execDir tm = do
  cdata <- getCompileDataWith ["go"] False Cases tm
  now <- coreLift $ clockTime UTC
  let defs = namedDefs cdata
      outFile = "expr_run_" ++ (show $ seconds now) ++ "_" ++ (show $ nanoseconds now)
  _ <- compileGo execDir outFile defs $ forget cdata.mainExpr
  coreLift_ $ system $ execDir </> outFile
  pure ()

export
goCG : Codegen
goCG = MkCG compileExpr executeExpr Nothing Nothing

main : IO ()
main = do
  mainWithCodegens [("go", goCG)]

