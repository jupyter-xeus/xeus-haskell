module Repl.Compiler (
  compileModule,
  runAction,
  runResultName,
  runResultIdent,
  runBlock
) where

import qualified Prelude ()
import MHSPrelude
import Data.List (foldl')
import Data.Maybe (isJust)

import MicroHs.Compile (Cache, compileModuleP, compileToCombinators)
import MicroHs.CompileCache (cachedModules)
import MicroHs.SymTab (SymTab, Entry(..), stLookup, stEmpty)
import MicroHs.Desugar (LDef)
import MicroHs.Exp (Exp(Var))
import MicroHs.Expr (EModule(..), EDef(..), ImpType(..))
import MicroHs.Ident (Ident, mkIdent, qualIdent, unQualIdent)
import qualified MicroHs.IdentMap as IMap
import MicroHs.Parse (parse, pTopModule)
import MicroHs.StateIO (runStateIO)
import MicroHs.TypeCheck (TModule(..), tBindingsOf, Symbols)
import MicroHs.Translate (TranslateMap, translateMap, translateWithMap)
import MicroHs.Builtin (builtinMdl)
import Unsafe.Coerce (unsafeCoerce)

import Repl.Context
import Repl.Error
import Repl.Utils

runResultName :: String
runResultName = "runResult"

runResultIdent :: Ident
runResultIdent = mkIdent runResultName

runBlock :: String -> String
runBlock stmt = unlines
  [ runResultName ++ " :: IO ()"
  , runResultName ++ " = _printOrRun ("
  , indent stmt
  , "  )"
  ]

compileModule :: ReplCtx -> String -> IO (Either ReplError (TModule [LDef], Cache, Symbols))
compileModule ctx src =
  case parse pTopModule "<repl>" src of
    Left perr -> pure (Left (ReplParseError perr))
    Right mdl -> do
      r <- runStateIO (compileModuleP (rcFlags ctx) ImpNormal mdl) (rcCache ctx)
      let (((dmdl, syms, _, _, _), _), cache') = unsafeCoerce r
          cmdl = compileToCombinators dmdl
      pure (Right (cmdl, cache', syms))

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
