-- |
-- Module: Language.Hydrangea.CLI
--
-- Helpers for resolving CLI entrypoint behavior.
module Language.Hydrangea.CLI
  ( allTopLevelProcsFlag
  , mainFlag
  , resolvePruneToMain
  , selectProgramDecs
  ) where

import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Prune (pruneDecsToMain)
import Language.Hydrangea.Syntax (Dec)

mainFlag :: String
mainFlag = "--main"

allTopLevelProcsFlag :: String
allTopLevelProcsFlag = "--all-top-level-procs"

-- | Resolve whether the CLI should prune top-level declarations to the
-- dependency closure of @main@.
resolvePruneToMain :: [String] -> Either String Bool
resolvePruneToMain flags =
  case (mainFlag `elem` flags, allTopLevelProcsFlag `elem` flags) of
    (True, True) -> Left "Cannot combine --main with --all-top-level-procs."
    (False, True) -> Right False
    _ -> Right True

-- | Select the declarations that should be visible to the chosen CLI mode.
--
-- The default behavior matches @--main@: keep only the declarations reachable
-- from the final top-level binding named @main@. The opt-out flag preserves
-- the older behavior of keeping every top-level declaration.
selectProgramDecs :: [String] -> [Dec Range] -> Either String [Dec Range]
selectProgramDecs flags decs = do
  pruneToMain <- resolvePruneToMain flags
  if pruneToMain
    then pruneDecsToMain decs
    else Right decs
