module Main

import Compiler.Common
import Compiler.CompileExpr

import Core.Context

import Idris.Driver
import Idris.Syntax

import Idris2.Compiler.Go

export
compileExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (tmpDir : String) ->
  (outputDir : String) ->
  ClosedTerm ->
  (outfile : String) ->
  Core (Maybe String)
compileExpr c s _ outputDir tm outfile = do
  let out = outfile ++ ".go"
  cdata <- getCompileData False Cases tm
  let defs = namedDefs cdata
  compileGo outputDir out defs $ forget cdata.mainExpr

export
executeExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  (execDir : String) ->
  ClosedTerm -> 
  Core ()
executeExpr c s tmpDir tm = pure ()

export
goCG : Codegen
goCG = MkCG compileExpr executeExpr Nothing Nothing

main : IO ()
main = do
  mainWithCodegens [("go", goCG)]

