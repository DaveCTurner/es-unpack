module Config (Config(..), getConfig) where

import Options.Applicative

data Config = Config
  { cVersion              :: String
  , cMixedNodes           :: Int
  , cDedicatedMasterNodes :: Int
  , cDataOnlyNodes        :: Int
  } deriving (Show, Eq)

getConfig :: IO Config
getConfig = execParser $ info (config <**> helper)
  ( fullDesc
  <> progDesc "unpack and configure a bunch of Elasticsearch nodes"
  <> header "es-unpack - unpack and configure a bunch of Elasticsearch nodes")

config :: Parser Config
config = Config
  <$> strOption
    ( long "version"
    <> metavar "VERSION"
    <> help "Elasticsearch version to use")
  <*> option auto
    ( long "nodes"
    <> metavar "NODES"
    <> help "Number of mixed master-data nodes to configure"
    <> value 1
    <> showDefault)
  <*> option auto
    ( long "master-nodes"
    <> metavar "NODES"
    <> help "Number of dedicated master nodes to configure"
    <> value 0
    <> showDefault)
  <*> option auto
    ( long "data-nodes"
    <> metavar "NODES"
    <> help "Number of data-only nodes to configure"
    <> value 0
    <> showDefault)
