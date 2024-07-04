{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.CabalAdd
( missingDependenciesAction
 , missingDependenciesSuggestion
 , hiddenPackageAction
)
where

import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (CodeAction (CodeAction),
                                              CodeActionKind (CodeActionKind_QuickFix),
                                              Diagnostic (..), Uri)
import           Text.Regex.TDFA
import           Distribution.Client.Add


missingDependenciesAction :: Int -> Uri -> Diagnostic -> [CodeAction]
missingDependenciesAction maxCompletions uri diag =
  mkCodeAction <$> missingDependenciesSuggestion maxCompletions (_message diag)
  where
    mkCodeAction suggestedDep =
      let
        title = "Add dependency " <> suggestedDep
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Nothing) Nothing

missingDependenciesSuggestion :: Int -> T.Text -> [T.Text]
missingDependenciesSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package \8216(.*)\8217"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results

hiddenPackageAction
  :: Int -- ^ Maximum number of suggestions to return
  -> Uri -- ^ File for which the diagnostic was generated
  -> Diagnostic
  -> [CodeAction]
hiddenPackageAction = undefined

hiddenPackageSuggestion :: Int -> T.Text -> [T.Text]
hiddenPackageSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text
    regex = "It is a member of the package '.*'\nwhich is unusable due to missing dependencies:[\n ]*([:word:-.]*)"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results