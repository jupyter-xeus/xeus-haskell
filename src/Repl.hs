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
import Data.Text (pack, unpack)
import System.IO (putStrLn)

import Foreign.C.String (CString, peekCString, peekCStringLen, newCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (poke)

import Repl.Context
import Repl.Error
import Repl.Utils
import Repl.Analysis
import Repl.Compiler
import Repl.Executor

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

c_OK, c_ERR :: CInt
c_OK  = 0
c_ERR = -1

writeErrorCString :: Ptr CString -> String -> IO ()
writeErrorCString errPtr msg =
  if errPtr == nullPtr then pure ()
  else newCString msg >>= poke errPtr

--------------------------------------------------------------------------------
-- FFI exports
--------------------------------------------------------------------------------

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

mhsReplFree :: ReplHandle -> IO ()
mhsReplFree = freeStablePtr

mhsReplFreeCString :: CString -> IO ()
mhsReplFreeCString = free

--------------------------------------------------------------------------------
-- Unified runner
--------------------------------------------------------------------------------

runReplAction
  :: (ReplCtx -> String -> IO (Either ReplError ReplCtx))
  -> ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
runReplAction act h srcPtr srcLen errPtr = do
  ref <- deRefStablePtr h
  src <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  result <- try (act ctx src) :: IO (Either SomeException (Either ReplError ReplCtx))
  let normalized = case result of
        Left ex -> Left (ReplRuntimeError (displayException ex))
        Right r -> r
  case normalized of
    Left e -> writeErrorCString errPtr (prettyReplError e) >> pure c_ERR
    Right ctx' -> writeIORef ref ctx' >> pure c_OK

--------------------------------------------------------------------------------
-- Public FFI API
--------------------------------------------------------------------------------

mhsReplDefine :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplDefine = runReplAction replDefine

mhsReplRun :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplRun = runReplAction replRun

mhsReplExecute :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplExecute = runReplAction replExecute

mhsReplIsComplete :: ReplHandle -> CString -> CSize -> IO CString
mhsReplIsComplete h srcPtr srcLen = do
  ref <- deRefStablePtr h
  src <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  status <- replIsComplete ctx src
  newCString status

mhsReplInspect :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplInspect h srcPtr srcLen resPtr = do
  ref <- deRefStablePtr h
  name <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  result <- replInspect ctx name
  case result of
    Left e -> writeErrorCString resPtr (prettyReplError e) >> pure c_ERR
    Right info -> newCString info >>= poke resPtr >> pure c_OK

mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
mhsReplCanParseDefinition ptr len = do
  code <- peekSource ptr len
  pure $ if canParseDefinition code then 1 else 0

mhsReplCanParseExpression :: CString -> CSize -> IO CInt
mhsReplCanParseExpression ptr len = do
  code <- peekSource ptr len
  pure $ if canParseExpression code then 1 else 0

mhsReplCompletionCandidates :: ReplHandle -> IO ()
mhsReplCompletionCandidates h = do
  ref <- deRefStablePtr h
  ctx <- readIORef ref
  let source = currentDefsSource ctx
  let localIdents = completionCandidates source
  let allCandidates = nub (reservedIds ++ localIdents)
  mapM_ putStrLn allCandidates

--------------------------------------------------------------------------------
-- CString utilities
--------------------------------------------------------------------------------

peekSource :: CString -> CSize -> IO String
peekSource ptr len
  | ptr == nullPtr = pure ""
  | len == 0       = peekCString ptr
  | otherwise      = peekCStringLen (ptr, fromIntegral len)
