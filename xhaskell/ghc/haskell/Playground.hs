-- Adapted from GHC's ghc-api-browser test (BSD-3-Clause).
-- A persistent GHCi-style session exported to the browser worker.
module XeusHaskellGhc (xhaskellGhcMain) where

import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.Coerce
import Data.Foldable (toList)
import Data.IORef
import Data.List (isPrefixOf, nub, sort)
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Parser.Lexer (ParserOpts)
import GHC.Parser.Utils (isDecl, isImport, isStmt)
import GHC.Plugins hiding (exprType, typeKind)
import GHC.Runtime.Context (setInteractivePrintName)
import GHC.Runtime.Interpreter
import GHC.Utils.Exception
import GHC.Wasm.Prim

newtype JSFunction t = JSFunction JSVal
type ExportedRequestFunction = JSString -> JSString -> Int -> Int -> IO JSString
data KernelSession = KernelSession Session (IORef [String])
data CellPlan
  = DeclarationsOnly String
  | StatementOnly String
  | DeclarationsThenStatement String String

resultMarker, printerName :: String
resultMarker = "__XHASKELL_GHC_RESULT_83f31cbe__"
printerName = "xhaskellGhcInteractivePrint"

xhaskellGhcMain :: JSString -> JSString -> IO (JSFunction ExportedRequestFunction)
xhaskellGhcMain jsLibdir jsArgs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    libdir <- takeJSString jsLibdir
    args <- words <$> takeJSString jsArgs
    session <- Session <$> newIORef undefined
    flip reflectGhc session $ do
      initGhcMonad (Just libdir)
      flags <- getSessionDynFlags
      _ <- setSessionDynFlags flags
        { ghcMode = CompManager, backend = interpreterBackend,
          ghcLink = LinkInMemory, verbosity = 1 }
      applyFlags args
      setContext [IIDecl $ simpleImportDecl $ mkModuleName "Prelude"]
      installPrinter
    bindings <- newIORef []
    let kernel = KernelSession session bindings
    toRequestFunction $ \jsOperation jsCode cursor detail -> do
      operation <- takeJSString jsOperation
      code <- takeJSString jsCode
      response <- defaultErrorHandler defaultFatalMessager defaultFlushOut $
        flip reflectGhc session $ dispatch kernel operation code cursor detail
      pure $ toJSString $ jsonObject [("ok", "true"), ("payload", response)]

takeJSString :: JSString -> IO String
takeJSString value = do
  result <- evaluate $ fromJSString value
  freeJSVal $ coerce value
  pure result

applyFlags :: [String] -> Ghc ()
applyFlags args = do
  flags <- getSessionDynFlags
  logger <- getLogger
  (newFlags, leftovers, warnings) <- parseDynamicFlags logger flags $ map noLoc args
  unless (null leftovers) $ fail $ "unrecognized GHC options: " ++ unwords (map unLoc leftovers)
  _ <- setSessionDynFlags newFlags
  setInteractiveDynFlags newFlags
  updated <- getSessionDynFlags
  liftIO $ printOrThrowDiagnostics logger (initPrintConfig updated) (initDiagOpts updated) $
    GhcDriverMessage <$> warnings

installPrinter :: Ghc ()
installPrinter = do
  names <- runDecls $ unlines
    [ printerName ++ " :: Show a => a -> IO ()",
      printerName ++ " value = putStrLn " ++ show resultMarker ++ " >> print value" ]
  printer <- case names of
    name : _ -> pure name
    [] -> fail "unable to install the interactive printer"
  session <- getSession
  setSession session {hsc_IC = setInteractivePrintName (hsc_IC session) printer}
  flags <- getInteractiveDynFlags
  setInteractiveDynFlags flags {interactivePrint = Just printerName}

dispatch :: KernelSession -> String -> String -> Int -> Int -> Ghc String
dispatch kernel operation code cursor detail = case operation of
  "execute" -> executeCode kernel code
  "complete" -> completeCode code cursor
  "inspect" -> inspectCode code cursor detail
  "is_complete" -> isCompleteCode code
  "warmup" -> warmupSession kernel
  _ -> fail $ "unknown GHC request: " ++ operation

executeCode :: KernelSession -> String -> Ghc String
executeCode kernel source
  | all isSpace source = pure $ jsonObject []
  | ":" `isPrefixOf` dropWhile isSpace source = executeCommand kernel source
  | otherwise = do
      parser <- initParserOpts <$> getInteractiveDynFlags
      if isImport parser source then do
        imported <- IIDecl <$> parseImportDecl source
        getContext >>= setContext . (++ [imported])
        pure $ jsonObject []
      else case planCell parser source of
        Just (DeclarationsOnly declarations) ->
          executeDeclarations kernel declarations
        Just (StatementOnly statement) ->
          executeStatement kernel statement
        Just (DeclarationsThenStatement declarations statement) -> do
          executeDeclarations kernel declarations
          executeStatement kernel statement
        Nothing -> executeStatement kernel source

executeDeclarations :: KernelSession -> String -> Ghc String
executeDeclarations kernel source = do
  runDecls source >>= rememberBindings kernel
  pure $ jsonObject []

planCell :: ParserOpts -> String -> Maybe CellPlan
planCell parser source = case findMixedSplit 1 of
  Just mixed -> Just mixed
  Nothing
    | startsWithDeclaration parser source -> Just $ DeclarationsOnly source
    | isStmt parser source -> Just $ StatementOnly source
    | otherwise -> Nothing
  where
    sourceLines = lines source
    findMixedSplit splitIndex
      | splitIndex >= length sourceLines = Nothing
      | otherwise =
          let (declarationLines, statementLines) = splitAt splitIndex sourceLines
              declarations = unlines declarationLines
              statement = unlines statementLines
           in if startsWithDeclaration parser declarations &&
                not (all isSpace statement) &&
                not (startsWithDeclaration parser statement)
                then case executableStatement parser statement of
                  Just executable ->
                    Just $ DeclarationsThenStatement declarations executable
                  Nothing -> findMixedSplit (splitIndex + 1)
                else findMixedSplit (splitIndex + 1)

startsWithDeclaration :: ParserOpts -> String -> Bool
startsWithDeclaration parser source =
  isDecl parser source || isDecl parser firstLine || firstWord `elem`
    [ "class", "data", "default", "deriving", "foreign", "infix",
      "infixl", "infixr", "instance", "newtype", "pattern", "type" ]
  where
    firstLine = takeWhile (/= '\n') source
    firstWord = takeWhile (not . isSpace) $ dropWhile isSpace source

executableStatement :: ParserOpts -> String -> Maybe String
executableStatement parser source
  | length (lines source) > 1 && isStmt parser wrapped = Just wrapped
  | isStmt parser source = Just source
  | isStmt parser wrapped = Just wrapped
  | otherwise = Nothing
  where
    wrapped = "do\n" ++ unlines (map ("  " ++) (lines source))

executeStatement :: KernelSession -> String -> Ghc String
executeStatement kernel source = do
  result <- execStmt source execOptions
  case result of
    ExecComplete {execResult = Left exception} -> liftIO $ throwIO exception
    ExecComplete {execResult = Right names} -> rememberBindings kernel names
    ExecBreak {} -> fail "execution stopped at a breakpoint"
  pure $ jsonObject []

rememberBindings :: KernelSession -> [Name] -> Ghc ()
rememberBindings (KernelSession _ bindings) names =
  liftIO $ modifyIORef' bindings (nub . (++ map (showSDocUnsafe . ppr) names))

executeCommand :: KernelSession -> String -> Ghc String
executeCommand kernel source =
  let (command, rest) = break isSpace $ dropWhile isSpace source
      argument = dropWhile isSpace rest in
  case command of
    ":type" -> typeCommand argument
    ":t" -> typeCommand argument
    ":info" -> infoCommand argument
    ":i" -> infoCommand argument
    ":kind" -> kindCommand False argument
    ":k" -> kindCommand False argument
    ":kind!" -> kindCommand True argument
    ":set" -> setCommand argument
    ":show" -> showCommand kernel argument
    ":module" -> moduleCommand argument
    ":m" -> moduleCommand argument
    _ -> fail $ "unsupported GHCi command: " ++ command

typeCommand :: String -> Ghc String
typeCommand expression = do
  when (null expression) $ fail "usage: :type <expression>"
  expressionType <- exprType TM_Inst expression
  stdoutResponse $ text expression <+> text "::" <+> ppr expressionType

kindCommand :: Bool -> String -> Ghc String
kindCommand normalize expression = do
  when (null expression) $ fail "usage: :kind[!] <type>"
  (resolvedType, kind) <- typeKind normalize expression
  stdoutResponse $ if normalize
    then text expression <+> text "::" <+> ppr kind $$ text "=" <+> ppr resolvedType
    else text expression <+> text "::" <+> ppr kind

infoCommand :: String -> Ghc String
infoCommand query = do
  when (null query) $ fail "usage: :info <name>"
  names <- parseName query
  docs <- forM names $ \name -> do
    info <- getInfo True name
    pure $ maybe (ppr name) pprInfo info
  stdoutResponse $ vcat $ toList docs
  where
    pprInfo (thing, fixity, instances, familyInstances, _) =
      ppr thing $$ ppr fixity $$ vcat (map ppr instances) $$ vcat (map ppr familyInstances)

setCommand :: String -> Ghc String
setCommand argument
  | null argument = getInteractiveDynFlags >>= stdoutResponse . text . show . EnumSet.toList . extensionFlags
  | otherwise = do
      let options = words argument
      unless (all allowedFlag options) $
        fail "only -X..., -fprint-..., -fno-print-..., -W... and -w are supported by :set"
      applyFlags options
      installPrinter
      pure $ jsonObject []
  where
    allowedFlag option = option == "-w" ||
      any (\prefix -> prefix `isPrefixOf` option) ["-X", "-W", "-fprint-", "-fno-print-"]

showCommand :: KernelSession -> String -> Ghc String
showCommand (KernelSession _ bindingRef) argument = case words argument of
  ["imports"] -> getContext >>= stdoutResponse . vcat . map ppr
  ["bindings"] -> do
    bindings <- liftIO $ readIORef bindingRef
    rows <- forM bindings $ \binding -> do
      bindingType <- exprType TM_Inst binding
      pure $ text binding <+> text "::" <+> ppr bindingType
    stdoutResponse $ vcat rows
  _ -> fail "usage: :show imports | :show bindings"

moduleCommand :: String -> Ghc String
moduleCommand argument = do
  let pieces = words argument
      names = filter (\piece -> piece `notElem` ["+", "-", "*"]) pieces
  when (null names) $ fail "usage: :module [+|-] <module> ..."
  imports <- forM names $ \name -> do
    _ <- findModule (mkModuleName name) Nothing
    pure $ IIDecl $ simpleImportDecl $ mkModuleName name
  context <- getContext
  let newContext
        | "*" `elem` pieces = imports
        | "-" `elem` pieces = filter (not . importsModule names) context
        | otherwise = context ++ imports
  setContext newContext
  pure $ jsonObject []
  where
    importsModule names (IIDecl declaration) =
      moduleNameString (unLoc $ ideclName declaration) `elem` names
    importsModule _ _ = False

completeCode :: String -> Int -> Ghc String
completeCode code cursor = do
  names <- map (showSDocUnsafe . ppr) <$> getRdrNamesInScope
  let safeCursor = max 0 $ min cursor $ length code
      prefix = reverse $ takeWhile completionCharacter $ reverse $ take safeCursor code
      commands = [":type", ":t", ":info", ":i", ":kind", ":k", ":kind!", ":set", ":show", ":module", ":m"]
      matches = sort $ filter (prefix `isPrefixOf`) $ nub $ commands ++ names
  pure $ jsonObject
    [ ("matches", jsonArray $ map jsonString matches),
      ("cursor_start", show $ safeCursor - length prefix),
      ("cursor_end", show safeCursor) ]

completionCharacter :: Char -> Bool
completionCharacter character = isAlphaNum character || character `elem` "_'.:!"

inspectCode :: String -> Int -> Int -> Ghc String
inspectCode code cursor detail = do
  let safeCursor = max 0 $ min cursor $ length code
      left = reverse $ takeWhile completionCharacter $ reverse $ take safeCursor code
      right = takeWhile completionCharacter $ drop safeCursor code
      token = left ++ right
  if null token || ":" `isPrefixOf` token
    then pure $ jsonObject [("found", "false")]
    else do
      expressionType <- exprType TM_Inst token
      typeText <- render $ text token <+> text "::" <+> ppr expressionType
      extra <- if detail < 1 then pure "" else do
        names <- parseName token
        infos <- forM names $ \name ->
          getInfo True name >>= maybe (pure "") (render . ppr . infoThing)
        pure $ concatMap ("\n\n" ++) infos
      pure $ jsonObject
        [ ("found", "true"),
          ("data", jsonObject [("text/plain", jsonString $ typeText ++ extra)]) ]
  where infoThing (thing, _, _, _, _) = thing

isCompleteCode :: String -> Ghc String
isCompleteCode source = do
  parser <- initParserOpts <$> getInteractiveDynFlags
  let trimmed = dropWhile isSpace source
      status
        | null trimmed = "complete"
        | lexicallyIncomplete source = "incomplete"
        | ":" `isPrefixOf` trimmed = "complete"
        | isImport parser source || maybe False (const True) (planCell parser source) = "complete"
        | otherwise = "invalid"
  pure $ jsonObject [("status", jsonString status), ("indent", jsonString "  ")]

lexicallyIncomplete :: String -> Bool
lexicallyIncomplete source =
  let opens = length $ filter (\character -> character `elem` "([{") source
      closes = length $ filter (\character -> character `elem` ")]}") source
      finalWord = case words source of [] -> ""; values -> last values
   in opens > closes || finalWord `elem` ["=", "->", "do", "let", "where", "of", "then", "else"]

warmupSession :: KernelSession -> Ghc String
warmupSession (KernelSession _ bindings) = do
  savedBindings <- liftIO $ readIORef bindings
  _ <- execStmt "1 + 1" execOptions
  liftIO $ writeIORef bindings savedBindings
  pure $ jsonObject []

stdoutResponse :: SDoc -> Ghc String
stdoutResponse document = render document >>= \output ->
  pure $ jsonObject [("stdout", jsonString $ output ++ "\n")]
render :: SDoc -> Ghc String
render document = getInteractiveDynFlags >>= \flags -> pure (showSDoc flags document)

jsonObject :: [(String, String)] -> String
jsonObject fields = "{" ++ joinWith "," [jsonString key ++ ":" ++ value | (key, value) <- fields] ++ "}"
jsonArray :: [String] -> String
jsonArray values = "[" ++ joinWith "," values ++ "]"
jsonString :: String -> String
jsonString value = '"' : concatMap escape value ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\b' = "\\b"
    escape '\f' = "\\f"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape character = [character]
joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith separator (value : values) = value ++ concatMap (separator ++) values

foreign import javascript "wrapper"
  toRequestFunction :: ExportedRequestFunction -> IO (JSFunction ExportedRequestFunction)
foreign export javascript "xhaskellGhcMain"
  xhaskellGhcMain :: JSString -> JSString -> IO (JSFunction ExportedRequestFunction)
