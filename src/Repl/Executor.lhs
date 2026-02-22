This module is the operational orchestrator of the REPL. It composes analysis, compilation, execution, and symbol lookup into user-visible commands while preserving a single evolving context across requests.

Its structure can be read as a transition system over context:
$\delta : (ctx, input) \mapsto Either\ ReplError\ ctx'$.
Specialized transitions (\verb|replDefine|, \verb|replRun|, \verb|replExecute|, \verb|replIsComplete|) share this shape so they can be chained uniformly and reasoned about as one control algebra.

This design keeps policy explicit: parsing strategy lives in \verb|Repl.Analysis|, state storage in \verb|Repl.Context|, and compilation mechanics in \verb|Repl.Compiler|, while this module remains the place where end-user intent is interpreted and dispatched.

\begin{code}
module Repl.Executor (
  replDefine,
  replRun,
  replExecute,
  replInspect,
  replIsComplete
) where
import qualified Prelude ()
import MHSPrelude
import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import MicroHs.Ident (mkIdent)
import MicroHs.SymTab (stLookup, Entry(..))
import MicroHs.Expr (showExpr)
import MicroHs.TypeCheck (Symbols)
import System.IO (putStrLn)
import Repl.Context
import Repl.Error
import Repl.Utils
import Repl.Analysis
import Repl.Compiler
\end{code}

\verb|MetaCommand| represents REPL command directives that are not plain code execution. It currently covers \verb|:type| and \verb|:kind| command payloads.

\begin{code}
data MetaCommand
  = TypeOfExpr String
  | KindOfType String
\end{code}

\verb|Completion| models the cell completeness state consumed by frontends. It distinguishes complete, incomplete, and invalid snippets.

\begin{code}
data Completion
  = Complete
  | Incomplete
  | Invalid
\end{code}

\verb|andThen| composes two \verb|IO (Either ...)| steps in a left-biased manner. It forwards \verb|Left| immediately and applies the next action only on \verb|Right|.

\begin{code}
andThen
  :: IO (Either ReplError a)
  -> (a -> IO (Either ReplError b))
  -> IO (Either ReplError b)
andThen act next = do
  result <- act
  case result of
    Left err -> pure (Left err)
    Right val -> next val
\end{code}

\verb|right| lifts a pure value into the \verb|IO (Either ReplError ...)| success shape. It is a tiny helper used to reduce repetition in success branches.

\begin{code}
right :: a -> IO (Either ReplError a)
right = pure . Right
\end{code}

\verb|replDefine| appends a definition snippet and recompiles the full definition module. On success it updates stored definitions, compile cache, and symbol tables.

\begin{code}
replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet =
  case appendDefinition ctx (ensureTrailingNewline snippet) of
    Left err -> pure (Left err)
    Right defsWithNew ->
      compileModule ctx (moduleFromDefs defsWithNew) `andThen`
        \(_, cache', syms') ->
          right ctx{ rcDefs = defsWithNew, rcCache = cache', rcSyms = syms' }
\end{code}

\verb|replRun| executes a statement/expression in the current context without mutating stored definitions. It compiles a generated run block and executes \verb|runResultIdent|.

\begin{code}
replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt =
  compileModule ctx (moduleSourceWith ctx (runBlock stmt)) `andThen`
    \(cmdl, cache', syms') ->
      runAction cache' cmdl runResultIdent `andThen`
        \() -> right ctx{ rcCache = cache', rcSyms = syms' }
\end{code}

\verb|replIsComplete| classifies snippet completeness for notebook frontend behavior. It checks empty input, valid splitability, and structural incompleteness heuristics.

\begin{code}
replIsComplete :: ReplCtx -> String -> IO String
replIsComplete ctx snippet = pure (toText completion)
  where
    completion
      | all isws snippet = Complete
      | hasValidSplit = Complete
      | isIncomplete snippet = Incomplete
      | otherwise = Invalid
    hasValidSplit = isJust (firstValidSplitPlan (currentDefsSource ctx) snippet)
    toText state =
      case state of
        Complete -> "complete"
        Incomplete -> "incomplete"
        Invalid -> "invalid"
\end{code}

\verb|replInspect| resolves a symbol from value/type tables and renders its signature. It tries value scope first, then type scope, and returns a runtime error if absent.

\begin{code}
replInspect :: ReplCtx -> String -> IO (Either ReplError String)
replInspect ctx name = do
  let ident = mkIdent name
      (typeTable, valueTable) = rcSyms ctx
      valueHit = lookupRendered "value" ident valueTable name
      typeHit = lookupRendered "type" ident typeTable name
      notFound = ReplRuntimeError ("Identifier not found: " ++ name)
  pure (maybe (Left notFound) Right (valueHit <|> typeHit))
\end{code}

\verb|replExecute| is the main execution entrypoint for snippets. It routes meta commands first, then tries split-plan execution strategies.

\begin{code}
replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet
  | Just cmd <- parseMetaCommand snippet = runMetaCommand ctx cmd
replExecute ctx snippet =
  case firstValidSplitPlan (currentDefsSource ctx) snippet of
    Nothing -> pure (Left (ReplParseError "unable to parse snippet"))
    Just (SplitDefineOnly defPart) -> replDefine ctx defPart
    Just (SplitDefineThenRun defPart runPart) -> defineThenRun ctx defPart runPart
\end{code}

\verb|runMetaCommand| dispatches parsed meta commands to their handlers. It keeps branching logic centralized for command extensions.

\begin{code}
runMetaCommand :: ReplCtx -> MetaCommand -> IO (Either ReplError ReplCtx)
runMetaCommand ctx cmd =
  case cmd of
    TypeOfExpr expr -> replTypeOf ctx expr
    KindOfType ty -> replKindOf ctx ty
\end{code}

\verb|replTypeOf| infers and prints the type of an expression via a probe definition. It generates a temporary binding, compiles it, and reads the inferred value symbol.

\begin{code}
replTypeOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replTypeOf ctx expr =
  runTypedProbe
    ctx
    ":type expects an expression"
    "failed to infer expression type"
    trimmedExpr
    src
    inferType
  where
    trimmedExpr = trimWs expr
    probeName = "xhReplTypeProbe"
    probeIdent = mkIdent probeName
    probeDef = unlines
      [ probeName ++ " = ("
      , indent trimmedExpr
      , "  )"
      ]
    src = moduleSourceWith ctx probeDef
    inferType (_, valueTable) =
      case stLookup "value" probeIdent valueTable of
        Right (Entry _ sigma) -> Just (showExpr sigma)
        Left _ -> Nothing
\end{code}

\verb|replKindOf| infers and prints the kind of a type expression. It compiles a temporary type alias probe and reads the inferred type symbol.

\begin{code}
replKindOf :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replKindOf ctx ty =
  runTypedProbe
    ctx
    ":kind expects a type"
    "failed to infer type kind"
    trimmedType
    src
    inferKind
  where
    trimmedType = trimWs ty
    probeName = "XhReplKindProbe"
    probeIdent = mkIdent probeName
    probeDef = "type " ++ probeName ++ " = " ++ trimmedType ++ "\n"
    src = moduleSourceWith ctx probeDef
    inferKind (typeTable, _) =
      case stLookup "type" probeIdent typeTable of
        Right (Entry _ kind) -> Just (showExpr kind)
        Left _ -> Nothing
\end{code}

\verb|parseMetaCommand| parses command prefixes and returns a structured command value. It trims input and applies boundary-aware keyword matching.

\begin{code}
parseMetaCommand :: String -> Maybe MetaCommand
parseMetaCommand raw =
  parseWith ":type" TypeOfExpr snippet <|> parseWith ":kind" KindOfType snippet
  where
    snippet = trimWs raw
    parseWith keyword ctor s = ctor <$> matchKeywordPrefix keyword s
\end{code}

\verb|defineThenRun| executes a split snippet by defining first and running second. It composes \verb|replDefine| and \verb|replRun| through \verb|andThen|.

\begin{code}
defineThenRun :: ReplCtx -> String -> String -> IO (Either ReplError ReplCtx)
defineThenRun ctx defPart runPart = replDefine ctx defPart `andThen` \ctx' -> replRun ctx' runPart
\end{code}

\verb|runTypedProbe| is the shared engine behind \verb|:type| and \verb|:kind| probing. It validates input, compiles probe source, extracts inferred text, prints it, and keeps context unchanged on success.

\begin{code}
runTypedProbe
  :: ReplCtx
  -> String
  -> String
  -> String
  -> String
  -> (Symbols -> Maybe String)
  -> IO (Either ReplError ReplCtx)
runTypedProbe ctx emptyInputError inferError shown src infer
  | null shown = pure (Left (ReplRuntimeError emptyInputError))
  | otherwise =
      compileModule ctx src `andThen` \(_, _, syms) ->
        case infer syms of
          Nothing -> pure (Left (ReplRuntimeError inferError))
          Just inferred -> do
            putStrLn ("> " ++ shown ++ " :: " ++ inferred)
            right ctx
\end{code}

\verb|lookupRendered| formats one symbol-table hit as inspect text. It performs scoped lookup and renders \verb|name :: signature| when successful.

\begin{code}
lookupRendered scope ident table shown =
  case stLookup scope ident table of
    Right (Entry _ sigOrKind) -> Just (shown ++ " :: " ++ showExpr sigOrKind)
    Left _ -> Nothing
\end{code}
