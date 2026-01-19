module Repl.Error (
  ReplError(..),
  prettyReplError
) where

import qualified Prelude ()
import MHSPrelude

data ReplError
  = ReplParseError String
  | ReplCompileError String
  | ReplRuntimeError String
  deriving (Eq, Show)

prettyReplError :: ReplError -> String
prettyReplError e =
  case e of
    ReplParseError s   -> "Parse error: "   ++ s
    ReplCompileError s -> "Compile error: " ++ s
    ReplRuntimeError s -> "Runtime error: " ++ s
