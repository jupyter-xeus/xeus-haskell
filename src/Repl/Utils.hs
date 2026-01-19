module Repl.Utils (
  splitColon,
  indent,
  ensureTrailingNewline,
  isws,
  allwsLine,
  dropWhileEnd,
  buildModule,
  moduleHeader
) where

import qualified Prelude ()
import MHSPrelude
import Data.List (reverse, dropWhile)

splitColon :: String -> [String]
splitColon s = case break (== ':') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitColon rest

indent :: String -> String
indent = unlines . map ("  " ++) . lines

ensureTrailingNewline :: String -> String
ensureTrailingNewline s
  | null s         = "\n"
  | last s == '\n' = s
  | otherwise      = s ++ "\n"

isws :: Char -> Bool
isws c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

allwsLine :: String -> Bool
allwsLine = all isws

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

moduleHeader :: String
moduleHeader = unlines
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "import Data.Typeable"
  , "import Numeric"
  ]

buildModule :: String -> String
buildModule defs = moduleHeader ++ defs
