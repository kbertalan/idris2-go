module Idris2Command

import System

export
idris2Command : (def : String) -> IO String
idris2Command def = do
  Just idris2Env <- getEnv "IDRIS2"
    | Nothing => pure def
  pure idris2Env

