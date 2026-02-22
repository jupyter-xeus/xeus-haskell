This module defines the error algebra shared by parser, compiler, runtime, and foreign interface boundaries. The objective is not only to report failure, but to preserve enough stage information so that downstream layers can respond coherently.

We can regard \verb|ReplError| as a tagged union over pipeline phases:
$E = E_{\mathrm{parse}} + E_{\mathrm{compile}} + E_{\mathrm{runtime}}$.
This decomposition keeps operational diagnostics interpretable while still allowing uniform transport through \verb|Either ReplError a| and C-string-based FFI messages.

\begin{code}
module Repl.Error (
  ReplError(..),
  prettyReplError
) where
import qualified Prelude ()
import MHSPrelude
\end{code}

\verb|ReplError| classifies failures by pipeline stage. Separating parse, compile, and runtime failures makes error handling and messaging clearer across module boundaries.

\begin{code}
data ReplError
  = ReplParseError String
  | ReplCompileError String
  | ReplRuntimeError String
  deriving (Eq, Show)
\end{code}

\verb|prettyReplError| converts structured errors into plain, user-facing text. It prepends a stable stage prefix while preserving the original detail payload.

\begin{code}
prettyReplError :: ReplError -> String
prettyReplError e =
  case e of
    ReplParseError s   -> "Parse error: "   ++ s
    ReplCompileError s -> "Compile error: " ++ s
    ReplRuntimeError s -> "Runtime error: " ++ s
\end{code}
