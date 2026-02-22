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

data MetaCommand
  = TypeOfExpr String
  | KindOfType String

data Completion
  = Complete
  | Incomplete
  | Invalid

andThen
  :: IO (Either ReplError a)
  -> (a -> IO (Either ReplError b))
  -> IO (Either ReplError b)
andThen act next = do
  result <- act
  case result of
    Left err -> pure (Left err)
    Right val -> next val

right :: a -> IO (Either ReplError a)
right = pure . Right

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet =
  case appendDefinition ctx (ensureTrailingNewline snippet) of
    Left err -> pure (Left err)
    Right defsWithNew ->
      compileModule ctx (moduleFromDefs defsWithNew) `andThen`
        \(_, cache', syms') ->
          right ctx{ rcDefs = defsWithNew, rcCache = cache', rcSyms = syms' }

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt =
  compileModule ctx (moduleSourceWith ctx (runBlock stmt)) `andThen`
    \(cmdl, cache', syms') ->
      runAction cache' cmdl runResultIdent `andThen`
        \() -> right ctx{ rcCache = cache', rcSyms = syms' }

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

replInspect :: ReplCtx -> String -> IO (Either ReplError String)
replInspect ctx name = do
  let ident = mkIdent name
      (typeTable, valueTable) = rcSyms ctx
      valueHit = lookupRendered "value" ident valueTable name
      typeHit = lookupRendered "type" ident typeTable name
      notFound = ReplRuntimeError ("Identifier not found: " ++ name)
  pure (maybe (Left notFound) Right (valueHit <|> typeHit))

replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet
  | Just cmd <- parseMetaCommand snippet = runMetaCommand ctx cmd
replExecute ctx snippet =
  case firstValidSplitPlan (currentDefsSource ctx) snippet of
    Nothing -> pure (Left (ReplParseError "unable to parse snippet"))
    Just (SplitDefineOnly defPart) -> replDefine ctx defPart
    Just (SplitDefineThenRun defPart runPart) -> defineThenRun ctx defPart runPart

runMetaCommand :: ReplCtx -> MetaCommand -> IO (Either ReplError ReplCtx)
runMetaCommand ctx cmd =
  case cmd of
    TypeOfExpr expr -> replTypeOf ctx expr
    KindOfType ty -> replKindOf ctx ty

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

parseMetaCommand :: String -> Maybe MetaCommand
parseMetaCommand raw =
  parseWith ":type" TypeOfExpr snippet <|> parseWith ":kind" KindOfType snippet
  where
    snippet = trimWs raw

    parseWith keyword ctor s = ctor <$> matchKeywordPrefix keyword s

defineThenRun :: ReplCtx -> String -> String -> IO (Either ReplError ReplCtx)
defineThenRun ctx defPart runPart = replDefine ctx defPart `andThen` \ctx' -> replRun ctx' runPart

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

lookupRendered scope ident table shown =
  case stLookup scope ident table of
    Right (Entry _ sigOrKind) -> Just (shown ++ " :: " ++ showExpr sigOrKind)
    Left _ -> Nothing
