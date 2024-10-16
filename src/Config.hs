module Config (Config(..), Target(..), getConfig) where

import Options.Applicative
import Data.List.Split

data Target
  = TargetVersion String
  | TargetTarball FilePath
  | TargetDistribution
  | NoTarget
  deriving (Show, Eq)

data Config = Config
  { cTarget               :: Target
  , cMixedNodes           :: Int
  , cDedicatedMasterNodes :: Int
  , cDataOnlyNodes        :: Int
  , cPortOffset           :: Int
  , cSecured              :: Bool
  , cWithRepo             :: Bool
  , cWithBootstrap        :: Bool
  , cWithGeoIp            :: Bool
  , cEnableDiskWatermarks :: Bool
  , cExtraDiscoveryPorts  :: [Int]
  , cExtraSettings        :: [String]
  } deriving (Show, Eq)

getConfig :: IO Config
getConfig = execParser $ info (config <**> helper)
  ( fullDesc
  <> progDesc "unpack and configure a bunch of Elasticsearch nodes"
  <> header "es-unpack - unpack and configure a bunch of Elasticsearch nodes")

versionOrTarball :: Parser Target
versionOrTarball = TargetVersion <$> version <|> TargetTarball <$> tarball <|> distribution
  where
    version = strOption
      ( long "version"
      <> metavar "VERSION"
      <> help "Elasticsearch version to use")
    tarball = strOption
      ( long "tarball"
      <> metavar "PATH"
      <> help "Elasticsearch tarball to use")
    distribution = flag NoTarget TargetDistribution
      ( long "distribution"
      <> help "Find tarball in source tree")

config :: Parser Config
config = Config
  <$> versionOrTarball
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
  <*> option auto
    ( long "port-offset"
    <> metavar "OFFSET"
    <> help "Offset to apply to the transport/HTTP ports for each node"
    <> value 0
    <> showDefault)
  <*> switch
    (  long "secured"
    <> help "Whether to configure TLS on this cluster")
  <*> (not <$> switch
    (  long "no-repo"
    <> help "If set, do not set up a local snapshot repository on this cluster"))
  <*> (not <$> switch
    (  long "no-bootstrap"
    <> help "If set, do not configure cluster.initial_master_nodes on this cluster"))
  <*> switch
    (  long "auto-download-geoip"
    <> help "If set, enable automatic downloads of GeoIP databases")
  <*> switch
    (  long "enable-disk-watermarks"
    <> help "If set, use the default disk watermark levels (otherwise set them all to 1B)")
  <*> (parsePorts <$> strOption
    ( long "extra-discovery-ports"
    <> metavar "PORTS"
    <> help "Extra ports to include in discovery config"
    <> value ""))
  <*> many (strOption
      ( long "extra-setting"
      <> metavar "SETTING"
      <> help "Additional setting to add to each node"))

parsePorts :: String -> [Int]
parsePorts = map read . filter (/= "") . splitOn ","
