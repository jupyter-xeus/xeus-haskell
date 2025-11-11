{-# LANGUAGE ForeignFunctionInterface #-}

module Repl (
  mhsReplNew,
  mhsReplFree,
  mhsReplDefine,
  mhsReplRun,
  mhsReplFreeCString
) where

import qualified Prelude ()
import MHSPrelude

import Control.Exception (try, SomeException, displayException)
import Data.IORef
import Data.List (intercalate)
import Foreign.C.String (CString, peekCString, peekCStringLen, newCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (poke)
import MicroHs.Compile (Cache, compileModuleP, compileToCombinators, emptyCache, getMhsDir)
import MicroHs.CompileCache (cachedModules)
import MicroHs.Desugar (LDef)
import MicroHs.Exp (Exp(Var))
import MicroHs.Expr (ImpType(..))
import MicroHs.Flags (Flags, defaultFlags)
import MicroHs.Ident (Ident, mkIdent, qualIdent)
import MicroHs.Parse (parse, pTopModule)
import MicroHs.StateIO (runStateIO)
import MicroHs.TypeCheck (TModule(..), tBindingsOf)
import MicroHs.Translate (translateMap, translateWithMap)
import Unsafe.Coerce (unsafeCoerce)

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
-- Context
--------------------------------------------------------------------------------

type ReplHandle = StablePtr (IORef ReplCtx)

data ReplCtx = ReplCtx
  { rcFlags :: Flags
  , rcCache :: Cache
  , rcDefs  :: String
  }

initialCtx :: IO ReplCtx
initialCtx = do
  dir <- getMhsDir
  let flags = defaultFlags dir
  pure ReplCtx { rcFlags = flags, rcCache = emptyCache, rcDefs = "" }

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- FFI exports
--------------------------------------------------------------------------------

foreign export ccall "mhs_repl_new"         mhsReplNew          :: IO ReplHandle
foreign export ccall "mhs_repl_free"        mhsReplFree         :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_define"      mhsReplDefine       :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_run"         mhsReplRun          :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_free_cstr"   mhsReplFreeCString  :: CString -> IO ()

mhsReplNew :: IO ReplHandle
mhsReplNew = newIORef =<< initialCtx >>= newStablePtr

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
  result <- act ctx src
  case result of
    Left e -> writeErrorCString errPtr (prettyReplError e) >> pure c_ERR
    Right ctx' -> writeIORef ref ctx' >> pure c_OK

--------------------------------------------------------------------------------
-- Compilation / execution
--------------------------------------------------------------------------------

moduleHeader :: String
moduleHeader = intercalate "\n"
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "default Num (Integer, Double)"
  , "default IsString (String)"
  , "default Show (())"
  ] ++ "\n"

runResultName :: String
runResultName = "runResult"

runResultIdent :: Ident
runResultIdent = mkIdent runResultName

buildModule :: String -> String
buildModule defs = moduleHeader ++ defs

runBlock :: String -> String
runBlock stmt = unlines
  [ runResultName ++ " :: IO ()"
  , runResultName ++ " = _printOrRun (" ++ stmt ++ ")"
  ]

ensureTrailingNewline :: String -> String
ensureTrailingNewline s
  | null s         = "\n"
  | last s == '\n' = s
  | otherwise      = s ++ "\n"


compileModule :: ReplCtx -> String -> IO (Either ReplError (TModule [LDef], Cache))
compileModule ctx src =
  case parse pTopModule "<repl>" src of
    Left perr -> pure (Left (ReplParseError perr))
    Right mdl -> do
      r <- runStateIO (compileModuleP (rcFlags ctx) ImpNormal mdl) (rcCache ctx)
      let (((dmdl, _, _, _, _), _), cache') = unsafeCoerce r
          cmdl = compileToCombinators dmdl
      pure (Right (cmdl, cache'))

runAction :: Cache -> TModule [LDef] -> Ident -> IO (Either ReplError ())
runAction cache cmdl ident = do
  let defs      = tBindingsOf cmdl
      baseMap   = translateMap $ concatMap tBindingsOf (cachedModules cache)
      actionAny = translateWithMap baseMap (defs, Var (qualIdent (tModuleName cmdl) ident))
      action    = unsafeCoerce actionAny :: IO ()
  action
  pure (Right ())

--------------------------------------------------------------------------------
-- REPL logic
--------------------------------------------------------------------------------

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet = do
  let snippet' = ensureTrailingNewline snippet
      defs'    = rcDefs ctx ++ snippet'
      src      = buildModule defs'
  cm <- compileModule ctx{ rcDefs = defs' } src
  case cm of
    Left err          -> pure (Left err)
    Right (_, cache') -> pure (Right ctx{ rcDefs = defs', rcCache = cache' })

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt = do
  let block = runBlock stmt
      src = buildModule (rcDefs ctx ++ block)
  cm <- compileModule ctx src
  case cm of
    Left err -> pure (Left err)
    Right (cmdl, cache') -> do
      r <- runAction cache' cmdl runResultIdent
      case r of
        Left e  -> pure (Left e)
        Right _ -> pure (Right ctx{ rcCache = cache' })

--------------------------------------------------------------------------------
-- Public FFI API
--------------------------------------------------------------------------------

mhsReplDefine :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplDefine = runReplAction replDefine

mhsReplRun :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplRun = runReplAction replRun

--------------------------------------------------------------------------------
-- CString utilities
--------------------------------------------------------------------------------

peekSource :: CString -> CSize -> IO String
peekSource ptr len
  | ptr == nullPtr = pure ""
  | len == 0       = peekCString ptr
  | otherwise      = peekCStringLen (ptr, fromIntegral len)

