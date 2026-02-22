This module is the compilation core of the REPL pipeline. It turns synthesized source text into executable internal form and then resolves selected bindings for evaluation against cached module context.

Conceptually, it defines two transformations: a compile step
$\Gamma : (C, src) \mapsto (cmdl, C', \Sigma')$
and an execution step
$\rho : (C', cmdl, ident) \mapsto IO()$.
The first advances cache and symbols; the second realizes runtime effect by translating the chosen identifier through the accumulated binding map.

By isolating these operations, the surrounding executor can compose behaviors (define, run, probe) without duplicating compiler plumbing, and performance work can focus on cache evolution rather than call-site rewrites.

\begin{code}
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
\end{code}

\verb|runResultName| is the stable binding name used for generated run wrappers. Using a fixed symbol keeps downstream execution logic simple and deterministic.

\begin{code}
runResultName :: String
runResultName = "runResult"
\end{code}

\verb|runResultIdent| is the identifier form of \verb|runResultName|. It is precomputed to avoid repeated conversions when invoking compiled actions.

\begin{code}
runResultIdent :: Ident
runResultIdent = mkIdent runResultName
\end{code}

\verb|runBlock| wraps a statement in a synthetic \verb|IO ()| binding named \verb|runResult|. The wrapper ensures execution uses \verb|_printOrRun| consistently.

\begin{code}
runBlock :: String -> String
runBlock stmt = unlines
  [ runResultName ++ " :: IO ()"
  , runResultName ++ " = _printOrRun ("
  , indent stmt
  , "  )"
  ]
\end{code}

\verb|compileModule| parses and compiles source in the current REPL context. It runs the MicroHs compile pipeline with existing cache and returns compiled module, updated cache, and symbols.

\begin{code}
compileModule :: ReplCtx -> String -> IO (Either ReplError (TModule [LDef], Cache, Symbols))
compileModule ctx src =
  case parse pTopModule "<repl>" src of
    Left perr -> pure (Left (ReplParseError perr))
    Right mdl -> do
      r <- runStateIO (compileModuleP (rcFlags ctx) ImpNormal mdl) (rcCache ctx)
      let (((dmdl, syms, _, _, _), _), cache') = unsafeCoerce r
          cmdl = compileToCombinators dmdl
      pure (Right (cmdl, cache', syms))
\end{code}

\verb|runAction| executes one compiled identifier in IO. It builds a translation map from cached modules, resolves the target variable, and runs it as an \verb|IO ()| action.

\begin{code}
runAction :: Cache -> TModule [LDef] -> Ident -> IO (Either ReplError ())
runAction cache cmdl ident = do
  let defs      = tBindingsOf cmdl
      baseMap   = withBuiltinAliases $ translateMap $ concatMap tBindingsOf (cachedModules cache)
      actionAny = translateWithMap baseMap (defs, Var (qualIdent (tModuleName cmdl) ident))
      action    = unsafeCoerce actionAny :: IO ()
  action
  pure (Right ())
\end{code}

\verb|withBuiltinAliases| augments a translation map with qualified builtin aliases. It inserts aliases only when they are missing to avoid overriding existing entries.

\begin{code}
withBuiltinAliases :: TranslateMap -> TranslateMap
withBuiltinAliases mp = foldl' addAlias mp (IMap.toList mp)
  where
    addAlias acc (ident, val) =
      let aliasIdent = qualIdent builtinMdl (unQualIdent ident)
      in if aliasIdent == ident || isJust (IMap.lookup aliasIdent acc)
           then acc
           else IMap.insert aliasIdent val acc
\end{code}
