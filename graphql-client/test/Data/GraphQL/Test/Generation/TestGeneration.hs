module Data.GraphQL.Test.Generation.TestGeneration (testGeneration) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

testGeneration :: TestTree
testGeneration =
  testCase "Test generated code" $ do
    (code, _, _) <- readProcessWithExitCode "bash" ["test/Data/GraphQL/Test/Generation/run.sh", logFile] ""
    case code of
      ExitSuccess -> pure ()
      ExitFailure _ -> do
        output <- readFile logFile
        assertFailure $ "===== Test failed. Output: =====\n" <> output
  where
    logFile = "/tmp/graphql-client-generation-test.log"
