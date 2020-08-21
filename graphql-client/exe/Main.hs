{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (bsToExp, embedFile, getDir)
import Language.Haskell.TH (listE, runIO, tupE)
import Language.Haskell.TH.Syntax (addDependentFile)
import Options.Applicative
import Path
import Path.IO (ensureDir, resolveFile', withSystemTempDir)
import System.Process.Typed (proc, runProcess_)

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
graphqlCodegenScript = $(embedFile "js/graphql-codegen-haskell.js")

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

    runProcess_ $ proc nodeExe [toFilePath graphqlCodegen, toFilePath configFile]
  where
    writeFile' = ByteString.writeFile . toFilePath
