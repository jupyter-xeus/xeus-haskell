module Repl.Context (
  ReplCtx(..),
  StoredDef(..),
  initialCtx,
  defsSource,
  currentDefsSource,
  moduleSourceWith,
  moduleFromDefs,
  appendDefinition,
  ReplHandle
) where

import qualified Prelude ()
import MHSPrelude

import System.Environment (lookupEnv)
import Data.IORef
import Data.List (nub)
import Foreign.StablePtr (StablePtr)

import MicroHs.Flags (Flags, defaultFlags, paths)
import MicroHs.Compile (Cache, emptyCache)
import MicroHs.SymTab (SymTab, stEmpty)
import MicroHs.TypeCheck (Symbols)
import MicroHs.Ident (Ident)

import Repl.Error
import Repl.Utils
import Repl.Analysis (extractDefinitionNames)

type ReplHandle = StablePtr (IORef ReplCtx)

data StoredDef = StoredDef
  { sdCode :: String
  , sdNames :: [Ident]
  }

data ReplCtx = ReplCtx
  { rcFlags :: Flags
  , rcCache :: Cache
  , rcDefs  :: [StoredDef]
  , rcSyms  :: Symbols
  }

initialCtx :: String -> IO ReplCtx
initialCtx dir = do
  let flags = defaultFlags dir
  mpath <- lookupEnv "MHS_LIBRARY_PATH"
  let rpath = maybe "." id mpath
      extra = splitColon rpath
      rcFlags = flags { paths = paths flags ++ extra }
  pure ReplCtx { rcFlags = rcFlags, rcCache = emptyCache, rcDefs = [], rcSyms = (stEmpty, stEmpty) }

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
