This module defines the persistent memory model of the notebook session. It is the place where a sequence of user cells is transformed into a coherent evolving context rather than a collection of unrelated compile requests.

At a formal level, we treat context as
$ReplCtx = (F, C, D, \Sigma)$,
where $F$ are compiler flags and paths, $C$ is compile cache, $D$ is ordered stored definitions, and $\Sigma$ is symbol information for inspection and completion. All higher-level REPL operations are meaningful only through controlled updates to this tuple.

The key policy encoded here is redefinition shadowing. If a new snippet introduces names $N$, then old stored definitions that bind any element of $N$ are removed before append, ensuring that the latest binding governs future execution while unrelated definitions remain stable.

\begin{code}
module Repl.Context (
  ReplCtx(..),
  StoredDef(..),
  initialCtx,
  defsSource,
  currentDefsSource,
  moduleSourceWith,
  moduleFromDefs,
  appendDefinition,
  ReplHandle
) where
import qualified Prelude ()
import MHSPrelude
import System.Environment (lookupEnv)
import Data.IORef
import Data.List (nub)
import Foreign.StablePtr (StablePtr)
import MicroHs.Flags (Flags, defaultFlags, paths)
import MicroHs.Compile (Cache, emptyCache)
import MicroHs.SymTab (SymTab, stEmpty)
import MicroHs.TypeCheck (Symbols)
import MicroHs.Ident (Ident)
import Repl.Error
import Repl.Utils
import Repl.Analysis (extractDefinitionNames)
\end{code}

\verb|ReplHandle| is the FFI-stable pointer type used to hold mutable REPL state. It wraps \verb|IORef ReplCtx| so updates can happen across calls while keeping one handle.

\begin{code}
type ReplHandle = StablePtr (IORef ReplCtx)
\end{code}

\verb|StoredDef| stores one user definition snippet and the identifiers it binds. This pairing lets the system remove obsolete definitions when names are redefined.

\begin{code}
data StoredDef = StoredDef
  { sdCode :: String
  , sdNames :: [Ident]
  }
\end{code}

\verb|ReplCtx| is the full mutable runtime state for the REPL session. It carries compile flags, incremental cache, stored definitions, and symbol tables.

\begin{code}
data ReplCtx = ReplCtx
  { rcFlags :: Flags
  , rcCache :: Cache
  , rcDefs  :: [StoredDef]
  , rcSyms  :: Symbols
  }
\end{code}

\verb|initialCtx| builds the starting context from runtime directory and environment paths. It extends default MicroHs search paths with \verb|MHS_LIBRARY_PATH| and initializes empty state containers.

\begin{code}
initialCtx :: String -> IO ReplCtx
initialCtx dir = do
  let flags = defaultFlags dir
  mpath <- lookupEnv "MHS_LIBRARY_PATH"
  let rpath = maybe "." id mpath
      extra = splitColon rpath
      rcFlags = flags { paths = paths flags ++ extra }
  pure ReplCtx { rcFlags = rcFlags, rcCache = emptyCache, rcDefs = [], rcSyms = (stEmpty, stEmpty) }
\end{code}

\verb|defsSource| concatenates raw source for a list of stored definitions. Order is preserved so later recompiles mirror user input sequence.

\begin{code}
defsSource :: [StoredDef] -> String
defsSource = concatMap sdCode
\end{code}

\verb|currentDefsSource| extracts concatenated definition source from the active context. It is a small projection helper used by parse and execution flows.

\begin{code}
currentDefsSource :: ReplCtx -> String
currentDefsSource = defsSource . rcDefs
\end{code}

\verb|moduleSourceWith| builds a complete module by appending extra source to current defs. It relies on \verb|buildModule| to inject required header/import scaffolding.

\begin{code}
moduleSourceWith :: ReplCtx -> String -> String
moduleSourceWith ctx extra = buildModule (currentDefsSource ctx ++ extra)
\end{code}

\verb|moduleFromDefs| builds a complete module from an explicit definition list. It is used when compiling updated definition sets.

\begin{code}
moduleFromDefs :: [StoredDef] -> String
moduleFromDefs defs = buildModule (defsSource defs)
\end{code}

\verb|stripRedefined| drops prior stored definitions that conflict with newly introduced names. A stored definition is kept only if all its bound names are absent from the replacement set.

\begin{code}
stripRedefined :: [StoredDef] -> [Ident] -> [StoredDef]
stripRedefined defs [] = defs
stripRedefined defs names = filter noOverlap defs
  where
    noOverlap def = all (`notElem` names) (sdNames def)
\end{code}

\verb|appendDefinition| parses new snippet names, removes conflicting older defs, and appends new definition metadata. This keeps the latest definition for each name while preserving unrelated definitions.

\begin{code}
appendDefinition :: ReplCtx -> String -> Either ReplError [StoredDef]
appendDefinition ctx snippet =
    case extractDefinitionNames snippet of
        Left err -> Left err
        Right names ->
            let uniqueNames = nub names
                retainedDefs = stripRedefined (rcDefs ctx) uniqueNames
            in Right (retainedDefs ++ [StoredDef snippet uniqueNames])
\end{code}
