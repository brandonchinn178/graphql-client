{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (bsToExp, embedFile, getDir)
import Language.Haskell.TH (listE, runIO, tupE)
import Language.Haskell.TH.Syntax (addDependentFile)
import Options.Applicative
import Path
import Path.IO (doesFileExist, ensureDir, resolveFile', withSystemTempDir)
import System.Exit (ExitCode(..))
import System.Process.Typed (proc, readProcess, runProcess_)

data CliOptions = CliOptions
  { cliNode   :: Maybe FilePath
  , cliConfig :: FilePath
  }

parseCliOptions :: Parser CliOptions
parseCliOptions = CliOptions
  <$> parseCliNode
  <*> parseCliConfig
  where
    parseCliNode = optional $ strOption $ mconcat
      [ long "node"
      , metavar "NODE"
      , help "Path to Node.JS executable (default to finding on PATH)"
      ]
    parseCliConfig = strOption $ mconcat
      [ long "config"
      , short 'c'
      , metavar "CONFIG"
      , help "Path to codegen.yml file (defaults to ./codegen.yml)"
      , value "./codegen.yml"
      ]

graphqlCodegenScript :: ByteString
graphqlCodegenScript = $(do
  let script = [relfile|js/graphql-codegen-haskell.js|]
      fallbackScript = [relfile|js/graphql-codegen-haskell-fallback.js|]

  scriptExists <- runIO $ doesFileExist script
  embedFile $ toFilePath $ if scriptExists then script else fallbackScript
  )

mockedLibraries :: [(Path Rel File, ByteString)]
mockedLibraries = $(do
  let dir = "js/mocks/"
  files <- runIO $ getDir dir
  listE $ flip map files $ \(fp, content) -> do
    addDependentFile $ dir ++ fp
    tupE [mkRelFile fp, bsToExp content]
  )

main :: IO ()
main = do
  CliOptions{..} <- execParser $ info (parseCliOptions <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Generate Haskell definitions from .graphql files"
    ]

  withSystemTempDir "graphql-codegen" $ \tmpDir -> do
    let graphqlCodegen = tmpDir </> [relfile|graphql-codegen-haskell.js|]
    writeFile' graphqlCodegen graphqlCodegenScript

    let nodeModulesDir = tmpDir </> [reldir|node_modules|]
    ensureDir nodeModulesDir
    forM_ mockedLibraries $ \(fp, content) ->
      writeFile' (nodeModulesDir </> fp) content

    nodeExe <- maybe (pure "node") (fmap toFilePath . resolveFile') cliNode
    configFile <- resolveFile' cliConfig

    configFileExists <- doesFileExist configFile
    unless configFileExists $
      errorWithoutStackTrace $ "Config file doesn't exist: " ++ toFilePath configFile

    try @SomeException (readProcess $ proc nodeExe ["-e", "console.log('TEST')"]) >>= \case
      Right (ExitSuccess, "TEST\n", _) -> return ()
      _ -> errorWithoutStackTrace $ "Could not find working Node executable: " ++ nodeExe

    runProcess_ $ proc nodeExe [toFilePath graphqlCodegen, toFilePath configFile]
  where
    writeFile' = ByteString.writeFile . toFilePath
