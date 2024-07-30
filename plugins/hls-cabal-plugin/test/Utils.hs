{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Utils where

import           Data.List                         (sort)
import qualified Data.Text                         as T
import           Ide.Plugin.Cabal                  (descriptor)
import qualified Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.Completion.Types
import           System.FilePath
import           Test.Hls
import           Ide.Types (defaultPluginDescriptor)


cabalPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalPlugin = mkPluginTestDescriptor descriptor "cabal"

ghcIdePlugin :: PluginTestDescriptor ()
ghcIdePlugin = mkPluginTestDescriptor (\_ pid -> defaultPluginDescriptor pid "ghcIdeTestPlugin") "core"

simpleCabalPrefixInfoFromPos :: Position -> T.Text -> CabalPrefixInfo
simpleCabalPrefixInfoFromPos pos prefix =
    CabalPrefixInfo
        { completionPrefix = prefix
        , completionCursorPosition = pos
        , isStringNotation = Nothing
        , completionRange = Range pos (Position 0 0)
        , completionWorkingDir = ""
        , completionFileName = "test"
        }

simpleCabalPrefixInfoFromFp :: T.Text -> FilePath -> CabalPrefixInfo
simpleCabalPrefixInfoFromFp prefix fp =
    CabalPrefixInfo
        { completionPrefix = prefix
        , isStringNotation = Nothing
        , completionCursorPosition = Position 0 0
        , completionRange = Range (Position 0 0) (Position 0 0)
        , completionWorkingDir = fp
        , completionFileName = "test"
        }

filePathComplTestDir :: FilePath
filePathComplTestDir = addTrailingPathSeparator $ testDataDir </> "filepath-completions"

runCabalTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runCabalTestCaseSession title subdir = testCase title . runCabalSession subdir

runHaskellTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runHaskellTestCaseSession title subdir = testCase title . runHaskellSession subdir

runCabalSession :: FilePath -> Session a -> IO a
runCabalSession subdir =
    failIfSessionTimeout . runSessionWithServer def cabalPlugin (testDataDir </> subdir)

runHaskellSession :: FilePath -> Session a -> IO a
runHaskellSession subdir =
    failIfSessionTimeout . runSessionWithServer def ghcIdePlugin (testDataDir </> subdir)

runCabalGoldenSession :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
runCabalGoldenSession title subdir fp act = goldenWithCabalDoc def cabalPlugin title testDataDir (subdir </> fp) "golden" "cabal" act

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-plugin" </> "test" </> "testdata"

-- | list comparison where the order in the list is irrelevant
(@?==) :: (HasCallStack, Ord a, Show a) => [a] -> [a] -> Assertion
(@?==) l1 l2 = sort l1 @?= sort l2
