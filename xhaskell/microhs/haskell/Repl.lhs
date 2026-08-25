The MicroHs REPL layer provides the continuity required by notebooks: each cell is evaluated against a persistent semantic context rather than in isolation. The C++ Xeus interpreter forwards requests through a small FFI surface, and this module translates those requests into explicit, auditable transitions over \verb|ReplCtx|.

A practical constraint of MicroHs is that startup work and incremental compilation cost must be managed carefully.
The implementation therefore keeps the FFI contract narrow and deterministic so cache reuse and error normalization remain predictable across repeated notebook interactions.

\subsection{Foreign-Function Interface Module}
This module is the semantic membrane between the C++ kernel process and the Haskell REPL engine. Its central task is to convert foreign calls into controlled state transitions over a long-lived \verb|ReplCtx| while preserving a stable, C-friendly contract for status codes and error buffers.

Formally, each exported endpoint implements a relation of the form
$\Delta_{\mathrm{ffi}} : (h, x) \mapsto (h', s, p)$,
where $h$ is a stable pointer to mutable session state, $x$ is decoded source input, $h'$ is the post-state handle, $s \in \{c\_OK, c\_ERR\}$ is a machine-level status code, and $p$ is optional textual payload. Query-style calls preserve state, while define/run/execute calls may advance cache and symbol tables.

The design objective is pragmatic: keep the foreign boundary narrow, deterministic, and explicit about ownership. This is why allocation and deallocation of returned C strings are encoded in the API itself, and why exception handling is normalized before crossing the language boundary.

\begin{code}
{-# LANGUAGE ForeignFunctionInterface #-}
module Repl (
  mhsReplNew,
  mhsReplFree,
  mhsReplDefine,
  mhsReplRun,
  mhsReplFreeCString,
  mhsReplCanParseDefinition,
  mhsReplCanParseExpression,
  mhsReplCompletionCandidates,
  mhsReplInspect,
  mhsReplIsComplete,
  mhsReplExecute
) where
import qualified Prelude ()
import MHSPrelude
import Control.Exception (try, SomeException, displayException)
import Data.IORef
import Data.List (nub)
import System.IO (putStrLn)
import Foreign.C.String (CString, peekCString, peekCStringLen, newCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (poke)
import Repl.Context
import Repl.Error
import Repl.Analysis
import Repl.Executor
\end{code}

\verb|c_OK| is the success status returned to C/C++ callers when an FFI action completes without error. It stays fixed at zero so callers can branch on a stable convention.

\begin{code}
c_OK, c_ERR :: CInt
c_OK  = 0
\end{code}

\verb|c_ERR| is the failure status returned to C/C++ callers when an FFI action fails. It stays fixed at \verb|-1| and pairs with an optional allocated error message.

\begin{code}
c_ERR = -1
\end{code}

\verb|writeErrorCString| stores a newly allocated error message into an output pointer when one is provided. The function is intentionally no-op for null pointers, allowing callers to omit error buffers safely.

\begin{code}
writeErrorCString :: Ptr CString -> String -> IO ()
writeErrorCString errPtr msg =
  if errPtr == nullPtr then pure ()
  else newCString msg >>= poke errPtr
\end{code}

\verb|mhsReplNew| creates a fresh REPL handle backed by an \verb|IORef ReplCtx|. It decodes the runtime path from C strings, initializes the context, and returns a stable pointer for cross-language ownership.

\begin{code}
foreign export ccall "mhs_repl_new"         mhsReplNew          :: CString -> CSize -> IO ReplHandle
foreign export ccall "mhs_repl_free"        mhsReplFree         :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_define"      mhsReplDefine       :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_run"         mhsReplRun          :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_execute"     mhsReplExecute      :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_is_complete" mhsReplIsComplete :: ReplHandle -> CString -> CSize -> IO CString
foreign export ccall "mhs_repl_free_cstr"   mhsReplFreeCString  :: CString -> IO ()
foreign export ccall "mhs_repl_can_parse_definition" mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
foreign export ccall "mhs_repl_can_parse_expression" mhsReplCanParseExpression :: CString -> CSize -> IO CInt
foreign export ccall "mhs_repl_completion_candidates" mhsReplCompletionCandidates :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_inspect" mhsReplInspect :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplNew :: CString -> CSize -> IO ReplHandle
mhsReplNew cstr csize = do
  str <- peekSource cstr csize
  ctx <- initialCtx str
  ref <- newIORef ctx
  newStablePtr ref
\end{code}

\verb|mhsReplFree| releases the stable pointer created by \verb|mhsReplNew|. It delegates directly to \verb|freeStablePtr|, keeping ownership semantics explicit.

\begin{code}
mhsReplFree :: ReplHandle -> IO ()
mhsReplFree = freeStablePtr
\end{code}

\verb|mhsReplFreeCString| frees strings previously allocated by this module and returned through FFI out-pointers. It calls the matching allocator-side \verb|free| so C++ can safely release messages.

\begin{code}
mhsReplFreeCString :: CString -> IO ()
mhsReplFreeCString = free
\end{code}

\verb|withHandleInput| is a shared adapter that decodes input and exposes both the context reference and decoded source string. This keeps repetitive pointer handling out of each endpoint implementation.

\begin{code}
withHandleInput
  :: ReplHandle
  -> CString
  -> CSize
  -> (IORef ReplCtx -> String -> IO a)
  -> IO a
withHandleInput h srcPtr srcLen k = do
  ref <- deRefStablePtr h
  src <- peekSource srcPtr srcLen
  k ref src
\end{code}

\verb|normalizeResult| folds both thrown exceptions and explicit \verb|ReplError| values into a single \verb|Either ReplError a| shape. That guarantees a uniform error path for all higher-level runners.

\begin{code}
normalizeResult
  :: Either SomeException (Either ReplError a)
  -> Either ReplError a
normalizeResult result =
  case result of
    Left ex -> Left (ReplRuntimeError (displayException ex))
    Right val -> val
\end{code}

\verb|runStatefulAction| executes an action that may mutate \verb|ReplCtx| and writes the updated state back on success. It catches exceptions, normalizes failures, and maps outcomes to \verb|c_OK|/\verb|c_ERR| plus optional error strings.

\begin{code}
runStatefulAction
  :: (ReplCtx -> String -> IO (Either ReplError ReplCtx))
  -> ReplHandle
  -> CString
  -> CSize
  -> Ptr CString
  -> IO CInt
runStatefulAction act h srcPtr srcLen errPtr =
  withHandleInput h srcPtr srcLen $ \ref src -> do
    ctx <- readIORef ref
    result <- try (act ctx src) :: IO (Either SomeException (Either ReplError ReplCtx))
    case normalizeResult result of
      Left err -> writeErrorCString errPtr (prettyReplError err) >> pure c_ERR
      Right ctx' -> writeIORef ref ctx' >> pure c_OK
\end{code}

\verb|runQueryAction| executes a read-style action that returns textual output rather than a new context. It applies the same error normalization flow as stateful actions while writing result text into the supplied output pointer.

\begin{code}
runQueryAction
  :: (ReplCtx -> String -> IO (Either ReplError String))
  -> ReplHandle
  -> CString
  -> CSize
  -> Ptr CString
  -> IO CInt
runQueryAction act h srcPtr srcLen outPtr =
  withHandleInput h srcPtr srcLen $ \ref src -> do
    ctx <- readIORef ref
    result <- try (act ctx src) :: IO (Either SomeException (Either ReplError String))
    case normalizeResult result of
      Left err -> writeErrorCString outPtr (prettyReplError err) >> pure c_ERR
      Right value -> newCString value >>= poke outPtr >> pure c_OK
\end{code}

\verb|mhsReplDefine| handles a definition snippet and persists resulting context state. It is a thin alias to the shared stateful runner with \verb|replDefine|.

\begin{code}
mhsReplDefine :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplDefine = runStatefulAction replDefine
\end{code}

\verb|mhsReplRun| executes a runnable snippet within the current context. It reuses \verb|runStatefulAction| so cache and symbols are updated consistently.

\begin{code}
mhsReplRun :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplRun = runStatefulAction replRun
\end{code}

\verb|mhsReplExecute| is the unified entrypoint that supports split plans and meta commands. It delegates to \verb|replExecute| through the shared stateful runner.

\begin{code}
mhsReplExecute :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplExecute = runStatefulAction replExecute
\end{code}

\verb|mhsReplIsComplete| reports whether input appears complete, incomplete, or invalid. It reads current context, computes status via \verb|replIsComplete|, and returns a newly allocated CString.

\begin{code}
mhsReplIsComplete :: ReplHandle -> CString -> CSize -> IO CString
mhsReplIsComplete h srcPtr srcLen =
  withHandleInput h srcPtr srcLen $ \ref src -> do
    ctx <- readIORef ref
    status <- replIsComplete ctx src
    newCString status
\end{code}

\verb|mhsReplInspect| resolves a symbol and returns rendered type/value information. It uses \verb|runQueryAction| with \verb|replInspect| to keep output/error handling uniform.

\begin{code}
mhsReplInspect :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplInspect = runQueryAction replInspect
\end{code}

\verb|mhsReplCanParseDefinition| is a fast parse probe for definition snippets. It decodes source and returns \verb|1|/\verb|0| to match C-friendly boolean conventions.

\begin{code}
mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
mhsReplCanParseDefinition ptr len = do
  code <- peekSource ptr len
  pure $ if canParseDefinition code then 1 else 0
\end{code}

\verb|mhsReplCanParseExpression| is the expression counterpart to the definition parse probe. It uses the same C-int boolean return convention.

\begin{code}
mhsReplCanParseExpression :: CString -> CSize -> IO CInt
mhsReplCanParseExpression ptr len = do
  code <- peekSource ptr len
  pure $ if canParseExpression code then 1 else 0
\end{code}

\verb|mhsReplCompletionCandidates| prints merged keyword and local identifier candidates. It reads current definitions, derives candidates, de-duplicates with \verb|nub|, and emits one candidate per line to stdout.

\begin{code}
mhsReplCompletionCandidates :: ReplHandle -> IO ()
mhsReplCompletionCandidates h = do
  ref <- deRefStablePtr h
  ctx <- readIORef ref
  let source = currentDefsSource ctx
  let localIdents = completionCandidates source
  let allCandidates = nub (reservedIds ++ localIdents)
  mapM_ putStrLn allCandidates
\end{code}

\verb|peekSource| turns C string input into a Haskell \verb|String| while honoring optional length semantics. It supports null pointers, null-terminated strings, and explicit-length buffers.

\begin{code}
peekSource :: CString -> CSize -> IO String
peekSource ptr len
  | ptr == nullPtr = pure ""
  | len == 0       = peekCString ptr
  | otherwise      = peekCStringLen (ptr, fromIntegral len)
\end{code}
