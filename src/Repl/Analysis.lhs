This module implements the syntactic decision procedure for interactive cells. In notebook use, one cell may mix declarations and executable expressions, so the runtime needs a principled way to infer intent without forcing users into a strict command grammar.

Operationally, the module blends parser-backed checks with lightweight lexical heuristics so that correctness and responsiveness coexist. Parser calls provide semantic authority for definition/expression validity, while token scanning supplies fast candidate extraction for completion.

The core abstraction is split search: for snippet $s$ with line indices $i$, we test partitions $(d_i, r_i)$ from largest prefix to smallest and accept the first valid plan. This greedy strategy approximates user intent in the common case where declarations precede executable tails.

\begin{code}
module Repl.Analysis (
  canParseDefinition,
  canParseExpression,
  extractDefinitionNames,
  SplitPlan(..),
  firstValidSplitPlan,
  trimWs,
  matchKeywordPrefix,
  completionCandidates,
  reservedIds,
  isIncomplete
) where
import qualified Prelude ()
import MHSPrelude
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import MicroHs.Parse (parse, pExprTop, pTopModule)
import MicroHs.Expr (EModule(..), EDef(..), patVars)
import MicroHs.Ident (Ident, SLoc(..))
import MicroHs.Lex (Token(..), lex)
import Repl.Error
import Repl.Utils (ensureTrailingNewline, buildModule, indent, allwsLine, isws)
\end{code}

\verb|SplitPlan| describes how a snippet can be interpreted by the executor. It captures either definition-only input or a split where definitions are followed by a runnable expression.

\begin{code}
data SplitPlan
  = SplitDefineOnly String
  | SplitDefineThenRun String String
\end{code}

\verb|canParseDefinition| checks whether a snippet is a valid set of top-level definitions. It wraps the snippet into a synthetic module and delegates parsing to MicroHs.

\begin{code}
canParseDefinition :: String -> Bool
canParseDefinition snippet =
  case parse pTopModule "<xhaskell-define>" (buildModule (ensureTrailingNewline snippet)) of
    Right _ -> True
    Left _  -> False
\end{code}

\verb|canParseExpression| checks whether a snippet is a valid expression. It parses with \verb|pExprTop| and maps parser success to a boolean.

\begin{code}
canParseExpression :: String -> Bool
canParseExpression snippet =
  case parse pExprTop "<xhaskell-expr>" snippet of
    Right _ -> True
    Left _  -> False
\end{code}

\verb|extractDefinitionNames| extracts bound identifiers from definition source. It parses as a module and delegates AST traversal to \verb|definitionNamesFromModule|.

\begin{code}
extractDefinitionNames :: String -> Either ReplError [Ident]
extractDefinitionNames snippet =
  case parse pTopModule "<xhaskell-define-names>" (buildModule snippet) of
    Left err -> Left (ReplParseError err)
    Right mdl -> Right (definitionNamesFromModule mdl)
\end{code}

\verb|definitionNamesFromModule| collects identifiers introduced by each top-level AST node. It pattern matches \verb|EDef| constructors and flattens all names into a single list.

\begin{code}
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
\end{code}

\verb|tokenize| produces lexical tokens from a source snippet. It uses MicroHs lexer with a default synthetic source location.

\begin{code}
tokenize :: String -> [Token]
tokenize = lex (SLoc "" 1 1)
\end{code}

\verb|extractIdents| filters tokens down to identifier names only. It uses list comprehension over \verb|TIdent| tokens and discards everything else.

\begin{code}
extractIdents :: [Token] -> [String]
extractIdents ts = [s | TIdent _ _ s <- ts]
\end{code}

\verb|completionCandidates| derives potential completion strings from source text. It composes tokenization and identifier extraction.

\begin{code}
completionCandidates :: String -> [String]
completionCandidates = extractIdents . tokenize
\end{code}

\verb|reservedIds| is the baseline keyword set offered for completion. It is a static list merged with user-defined identifiers.

\begin{code}
reservedIds :: [String]
reservedIds =
  [ "case", "class", "data", "default", "deriving", "do", "else"
  , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
  , "instance", "let", "module", "newtype", "of", "then", "type"
  , "where", "_"
  ]
\end{code}

\verb|isIncomplete| heuristically checks whether a snippet is unfinished. It tracks bracket nesting and quote states in one pass.

\begin{code}
isIncomplete :: String -> Bool
isIncomplete s = go [] s
  where
    go st [] = not (null st)
    go st (c:cs)
      | c `elem` "([{" = go (c:st) cs
      | c `elem` ")]}" = case st of
          [] -> False
          (x:xs) -> if match x c then go xs cs else False
      | c == '"' = goString st cs
      | c == '\'' = goChar st cs
      | otherwise = go st cs
    goString st [] = True
    goString st ('\\':'"':cs) = goString st cs
    goString st ('"':cs) = go st cs
    goString st (_:cs) = goString st cs
    goChar st [] = True
    goChar st ('\\':'\'':cs) = goChar st cs
    goChar st ('\'':cs) = go st cs
    goChar st (_:cs) = goChar st cs
    match '(' ')' = True
    match '[' ']' = True
    match '{' '}' = True
    match _ _ = False
\end{code}

\verb|firstValidSplitPlan| returns the first acceptable split strategy for a snippet. It tries split points from longest prefix to shortest and picks the first successful classification.

\begin{code}
firstValidSplitPlan :: String -> String -> Maybe SplitPlan
firstValidSplitPlan currentDefs snippet =
  let snippetLines = lines (ensureTrailingNewline snippet)
  in listToMaybe (mapMaybe (classifySplit currentDefs snippetLines) [length snippetLines, length snippetLines - 1 .. 0])
\end{code}

\verb|classifySplit| evaluates one split point and classifies it as define-only, define-then-run, or invalid. It validates definition prefixes and then checks candidate run segments as expression or \verb|do|-wrapped expression.

\begin{code}
classifySplit :: String -> [String] -> Int -> Maybe SplitPlan
classifySplit currentDefs snippetLines splitIndex
  | not (canParseDefinition candidateDefs) = Nothing
  | all allwsLine runLines = Just (SplitDefineOnly defPart)
  | canParseExpression runPart = Just (SplitDefineThenRun defPart runPart)
  | canParseExpression doRunPart = Just (SplitDefineThenRun defPart doRunPart)
  | otherwise = Nothing
  where
    (defLines, runLines) = splitAt splitIndex snippetLines
    defPart = unlines defLines
    runPart = unlines (dropWhileEndLocal allwsLine runLines)
    doRunPart = "do\n" ++ indent runPart
    candidateDefs = currentDefs ++ defPart
\end{code}

\verb|dropWhileEndLocal| removes a suffix that matches a predicate. It uses reverse-drop-reverse to avoid depending on library availability differences.

\begin{code}
dropWhileEndLocal :: (a -> Bool) -> [a] -> [a]
dropWhileEndLocal f = reverse . dropWhile f . reverse
\end{code}

\verb|trimWs| trims leading and trailing whitespace. It reuses \verb|isws| and reverse-based stripping for both ends.

\begin{code}
trimWs :: String -> String
trimWs = dropWhile isws . reverse . dropWhile isws . reverse
\end{code}

\verb|matchKeywordPrefix| recognizes command keywords like \verb|:type| only when followed by a word boundary. It prevents accidental partial matches such as \verb|:typed|.

\begin{code}
matchKeywordPrefix :: String -> String -> Maybe String
matchKeywordPrefix keyword snippet
  | startsWith keyword snippet && hasBoundary keyword snippet =
      Just (drop (length keyword) snippet)
  | otherwise = Nothing
  where
    hasBoundary key s =
      let rest = drop (length key) s
      in null rest || isws (head rest)
\end{code}

\verb|startsWith| checks whether text begins with a specific prefix. It uses length-based \verb|take| equality for a small, explicit helper.

\begin{code}
startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix
\end{code}
