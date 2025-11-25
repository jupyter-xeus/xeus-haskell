{-# LANGUAGE ForeignFunctionInterface #-}

module Repl (
  mhsReplNew,
  mhsReplFree,
  mhsReplDefine,
  mhsReplRun,
  mhsReplFreeCString,
  mhsReplCanParseDefinition,
  mhsReplCanParseExpression
) where

import qualified Prelude ()
import MHSPrelude

import Control.Exception (try, SomeException, displayException)
import Data.IORef
import Data.List (foldl', intercalate, nub)
import Data.Maybe (isJust)
import Foreign.C.String (CString, peekCString, peekCStringLen, newCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (poke)
import MicroHs.Builtin (builtinMdl)
import MicroHs.Compile (Cache, compileModuleP, compileToCombinators, emptyCache)
import MicroHs.CompileCache (cachedModules)
import MicroHs.Desugar (LDef)
import MicroHs.Exp (Exp(Var))
import MicroHs.Expr (EModule(..), EDef(..), ImpType(..), patVars)
import MicroHs.Flags (Flags, defaultFlags)
import MicroHs.Ident (Ident, mkIdent, qualIdent, unQualIdent)
import qualified MicroHs.IdentMap as IMap
import MicroHs.Parse (parse, pExprTop, pTopModule)
import MicroHs.StateIO (runStateIO)
import MicroHs.TypeCheck (TModule(..), tBindingsOf)
import MicroHs.Translate (TranslateMap, translateMap, translateWithMap)
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

data StoredDef = StoredDef
  { sdCode :: String
  , sdNames :: [Ident]
  }

data ReplCtx = ReplCtx
  { rcFlags :: Flags
  , rcCache :: Cache
  , rcDefs  :: [StoredDef]
  }

initialCtx :: String -> IO ReplCtx
initialCtx dir = do
  let flags = defaultFlags dir
  pure ReplCtx { rcFlags = flags, rcCache = emptyCache, rcDefs = [] }

defsSource :: [StoredDef] -> String
defsSource = concatMap sdCode

currentDefsSource :: ReplCtx -> String
currentDefsSource = defsSource . rcDefs

moduleSourceWith :: ReplCtx -> String -> String
moduleSourceWith ctx extra = buildModule (currentDefsSource ctx ++ extra)

moduleFromDefs :: [StoredDef] -> String
moduleFromDefs defs = buildModule (defsSource defs)

stripRedefined :: [StoredDef] -> [Ident] -> [StoredDef]
stripRedefined defs [] = defs
stripRedefined defs names = filter noOverlap defs
  where
    noOverlap def = all (`notElem` names) (sdNames def)

appendDefinition :: ReplCtx -> String -> Either ReplError [StoredDef]
appendDefinition ctx snippet =
    case extractDefinitionNames snippet of
        Left err -> Left err
        Right names ->
            let uniqueNames = nub names
                retainedDefs = stripRedefined (rcDefs ctx) uniqueNames
            in Right (retainedDefs ++ [StoredDef snippet uniqueNames])

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

foreign export ccall "mhs_repl_new"         mhsReplNew          :: CString -> CSize -> IO ReplHandle
foreign export ccall "mhs_repl_free"        mhsReplFree         :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_define"      mhsReplDefine       :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_run"         mhsReplRun          :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_execute"     mhsReplExecute      :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_free_cstr"   mhsReplFreeCString  :: CString -> IO ()
foreign export ccall "mhs_repl_can_parse_definition" mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
foreign export ccall "mhs_repl_can_parse_expression" mhsReplCanParseExpression :: CString -> CSize -> IO CInt

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
-- Compilation / execution
--------------------------------------------------------------------------------

moduleHeader :: String
moduleHeader = unlines
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "import Data.Typeable"
  ]

runResultName :: String
runResultName = "runResult"

runResultIdent :: Ident
runResultIdent = mkIdent runResultName

buildModule :: String -> String
buildModule defs = moduleHeader ++ defs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

runBlock :: String -> String
runBlock stmt = unlines
  [ runResultName ++ " :: IO ()"
  , runResultName ++ " = _printOrRun ("
  , indent stmt
  , "  )"
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
      baseMap   = withBuiltinAliases $ translateMap $ concatMap tBindingsOf (cachedModules cache)
      actionAny = translateWithMap baseMap (defs, Var (qualIdent (tModuleName cmdl) ident))
      action    = unsafeCoerce actionAny :: IO ()
  action
  pure (Right ())

withBuiltinAliases :: TranslateMap -> TranslateMap
withBuiltinAliases mp = foldl' addAlias mp (IMap.toList mp)
  where
    addAlias acc (ident, val) =
      let aliasIdent = qualIdent builtinMdl (unQualIdent ident)
      in if aliasIdent == ident || isJust (IMap.lookup aliasIdent acc)
           then acc
           else IMap.insert aliasIdent val acc

--------------------------------------------------------------------------------
-- REPL logic
--------------------------------------------------------------------------------

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet = do
  let snippet' = ensureTrailingNewline snippet
  case appendDefinition ctx snippet' of
    Left err -> pure (Left err)
    Right defsWithNew -> do
      cm <- compileModule ctx (moduleFromDefs defsWithNew)
      case cm of
        Left err'          -> pure (Left err')
        Right (_, cache') -> pure (Right ctx{ rcDefs = defsWithNew, rcCache = cache' })

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt = do
  let block = runBlock stmt
      src = moduleSourceWith ctx block
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

mhsReplExecute :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplExecute = runReplAction replExecute

replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet =
  let candidateDefs = currentDefsSource ctx ++ ensureTrailingNewline snippet
  in
    if canParseDefinition candidateDefs
      then replDefine ctx snippet
      else if canParseExpression snippet
        then replRun ctx snippet
        else pure (Left (ReplParseError "unable to parse snippet"))

--------------------------------------------------------------------------------
-- CString utilities
--------------------------------------------------------------------------------

peekSource :: CString -> CSize -> IO String
peekSource ptr len
  | ptr == nullPtr = pure ""
  | len == 0       = peekCString ptr
  | otherwise      = peekCStringLen (ptr, fromIntegral len)

--------------------------------------------------------------------------------
-- Parser helpers
--------------------------------------------------------------------------------

canParseDefinition :: String -> Bool
canParseDefinition snippet =
  case parse pTopModule "<xhaskell-define>" (buildModule (ensureTrailingNewline snippet)) of
    Right _ -> True
    Left _  -> False

canParseExpression :: String -> Bool
canParseExpression snippet =
  case parse pExprTop "<xhaskell-expr>" snippet of
    Right _ -> True
    Left _  -> False

extractDefinitionNames :: String -> Either ReplError [Ident]
extractDefinitionNames snippet =
  case parse pTopModule "<xhaskell-define-names>" (buildModule snippet) of
    Left err -> Left (ReplParseError err)
    Right mdl -> Right (definitionNamesFromModule mdl)

definitionNamesFromModule :: EModule -> [Ident]
definitionNamesFromModule (EModule _ _ defs) = concatMap definitionNames defs
  where
    definitionNames def =
      case def of
        Data lhs _ _        -> [lhsIdent lhs]
        Newtype lhs _ _     -> [lhsIdent lhs]
        Type lhs _          -> [lhsIdent lhs]
        Fcn name _          -> [name]
        PatBind pat _       -> patVars pat
        Sign names _        -> names
        KindSign name _     -> [name]
        Pattern lhs _ _     -> [lhsIdent lhs]
        Class _ lhs _ _     -> [lhsIdent lhs]
        DfltSign name _     -> [name]
        ForImp _ _ name _   -> [name]
        Infix _ names       -> names
        _                   -> []

    lhsIdent (name, _) = name

mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
mhsReplCanParseDefinition ptr len = do
  code <- peekSource ptr len
  pure $ if canParseDefinition code then 1 else 0

mhsReplCanParseExpression :: CString -> CSize -> IO CInt
mhsReplCanParseExpression ptr len = do
  code <- peekSource ptr len
  pure $ if canParseExpression code then 1 else 0
