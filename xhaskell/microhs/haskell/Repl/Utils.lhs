This module contains the small, deterministic utilities that make the larger REPL pipeline stable. Although each function is simple, together they enforce textual normalization and module-shape invariants relied on by parsing, split planning, and execution code generation.

The guiding principle is to keep low-level string and list transformations explicit and dependency-light. In practice, these helpers define canonicalization laws such as newline completion, indentation shaping, and module-header injection, so higher layers can assume normalized source structure.

\begin{code}
module Repl.Utils (
  splitColon,
  indent,
  ensureTrailingNewline,
  isws,
  allwsLine,
  dropWhileEnd,
  buildModule,
  moduleHeader
) where
import qualified Prelude ()
import MHSPrelude
import Data.List (reverse, dropWhile)
\end{code}

\verb|splitColon| splits a colon-separated path string into list elements. It uses recursive \verb|break| so behavior remains simple and dependency-free.

\begin{code}
splitColon :: String -> [String]
splitColon s = case break (== ':') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitColon rest
\end{code}

\verb|indent| prefixes each line with two spaces. This is used when embedding user statements into generated wrapper definitions.

\begin{code}
indent :: String -> String
indent = unlines . map ("  " ++) . lines
\end{code}

\verb|ensureTrailingNewline| guarantees source ends with a newline. It normalizes empty input to a single newline and appends one when missing.

\begin{code}
ensureTrailingNewline :: String -> String
ensureTrailingNewline s
  | null s         = "\n"
  | last s == '\n' = s
  | otherwise      = s ++ "\n"
\end{code}

\verb|isws| checks whether a character is recognized as whitespace in this module. The predicate is intentionally explicit and reused by completion/parse helpers.

\begin{code}
isws :: Char -> Bool
isws c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
\end{code}

\verb|allwsLine| checks whether an entire line is whitespace. It applies \verb|isws| to all characters in the line.

\begin{code}
allwsLine :: String -> Bool
allwsLine = all isws
\end{code}

\verb|dropWhileEnd| removes a trailing suffix matching a predicate. It uses reverse/dropWhile/reverse to avoid requiring extra library features.

\begin{code}
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse
\end{code}

\verb|moduleHeader| is the fixed prelude section for synthetic REPL modules. It imports exactly what generated evaluation wrappers rely on.

\begin{code}
moduleHeader :: String
moduleHeader = unlines
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "import Data.Typeable"
  , "import Numeric"
  ]
\end{code}

\verb|buildModule| combines \verb|moduleHeader| with accumulated definitions. It creates the full source unit sent through the compile pipeline.

\begin{code}
buildModule :: String -> String
buildModule defs = moduleHeader ++ defs
\end{code}
