{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad
import Data.Aeson
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Read (readMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Set as S

data Node = Node
  { nodeIndex         :: Int
  , nodeHttpPort      :: Int
  , nodeTransportPort :: Int
  , nodeIsMaster      :: Bool
  , nodeIsDataNode    :: Bool
  , nodeUnicastHosts  :: [String]
  , nodeMinimumMasterNodes :: Int
  } deriving (Show, Eq)

nodesFromConfig :: Config -> [Node]
nodesFromConfig config = result

  where
  result = zipWith ($) constructors [0..]

  constructors = replicate (cDedicatedMasterNodes config) masterNode
              ++ replicate (cMixedNodes           config) mixedNode
              ++ replicate (cDataOnlyNodes        config) dataNode

  discoveryPorts = S.toList $ S.fromList $ map nodeTransportPort (filter nodeIsMaster result) ++ cExtraDiscoveryPorts config

  unicastHosts = [ "127.0.0.1:" ++ show p | p <- discoveryPorts ]

  rawNode i = Node
    { nodeIndex              =        i
    , nodeHttpPort           = 9200 + i + cPortOffset config
    , nodeTransportPort      = 9300 + i + cPortOffset config
    , nodeIsMaster           = False
    , nodeIsDataNode         = False
    , nodeUnicastHosts       = unicastHosts
    , nodeMinimumMasterNodes = length unicastHosts `div` 2 + 1
    }

  dataNode   i = (rawNode i) { nodeIsDataNode = True }
  masterNode i = (rawNode i) { nodeIsMaster   = True }
  mixedNode  i = (rawNode i) { nodeIsDataNode = True, nodeIsMaster = True }

dropTgzExtension :: FilePath -> FilePath
dropTgzExtension fp
    | ext `elem` [".tar", ".gz"] = dropTgzExtension dropped
    | otherwise                  = fp
  where
  (dropped, ext) = splitExtension fp

resolveTarball :: Target -> IO FilePath
resolveTarball target@(TargetVersion version) = do
  tarballDir <- getEnv "ES_TARBALL_DIR"
  let tarballPath = tarballDir </> ("elasticsearch-" ++ version ++ ".tar.gz")

  tarballExists <- doesFileExist tarballPath
  unless tarballExists $ do
    putStrLn $ "Tarball " ++ tarballPath ++ " not found. Download with:"

    let urlPrefix
          | "-SNAPSHOT" `isSuffixOf` version = "https://snapshots.elastic.co/downloads/elasticsearch/elasticsearch-"
          | otherwise                        = "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-"

    maybePlatform <- lookupEnv "ES_TARBALL_PLATFORM"
    let suffix = case (maybePlatform, getVersion target) of
          (Just platform, Just (majorVersion, _)) | 7 <= majorVersion -> "-" ++ platform
          (Just platform, Nothing)                                    -> "-" ++ platform
          _                                                           -> ""

    putStrLn $ "curl -f " ++ urlPrefix ++ version ++ suffix ++ ".tar.gz -o '" ++ tarballPath
      ++ ".partial' && mv -v '" ++ tarballPath ++ ".partial' '" ++ tarballPath ++ "'"
    exitWith $ ExitFailure 1

  return tarballPath

resolveTarball (TargetTarball tarballPath) = do
  tarballExists <- doesFileExist tarballPath
  unless tarballExists $ do
    putStrLn $ "Tarball " ++ tarballPath ++ " not found."
    exitWith $ ExitFailure 1

  return tarballPath

resolveTarball TargetDistribution = do
  maybePlatform <- lookupEnv "ES_TARBALL_PLATFORM"
  (jobName, platform) <- case maybePlatform of
    Just "darwin-aarch64" -> return ("darwin-aarch64-tar", "darwin-aarch64")
    Just "darwin-x86_64" -> return ("darwin-tar", "darwin-x86_64")
    Just "linux-x86_64"  -> return ("linux-tar", "linux-x86_64")
    Just otherPlatform   -> do
      putStrLn $ "unknown platform ES_TARBALL_PLATFORM=" ++ otherPlatform
      exitWith $ ExitFailure 1
    Nothing -> do
      putStrLn $ "ES_TARBALL_PLATFORM unset"
      exitWith $ ExitFailure 1
  let tarballLocation = "distribution" </> "archives" </> jobName </> "build" </> "distributions"
      prefix = "elasticsearch-"
      suffix = "-SNAPSHOT-" ++ platform ++ ".tar.gz"
  locationExists <- doesDirectoryExist tarballLocation
  unless locationExists $ do
    putStrLn $ tarballLocation ++ " does not exist - are you in the Elasticsearch source tree? If yes, run the following command to build tarball:"
    putStrLn $ "./gradlew :distribution:archives:" ++ jobName ++ ":assemble"
    exitWith $ ExitFailure 1

  dirContents <- filter (\f -> prefix `isPrefixOf` f && suffix `isSuffixOf` f) <$> getDirectoryContents tarballLocation
  case dirContents of
    [] -> do
      putStrLn $ "no tarballs found matching " ++ tarballLocation ++ "/" ++ prefix ++ "*" ++ suffix ++ " - run the following command to build tarball:"
      putStrLn $ "./gradlew :distribution:archives:" ++ jobName ++ ":assemble"
      exitWith $ ExitFailure 1
    [f] -> do
      let tarballPath = tarballLocation </> f
      putStrLn $ "using distribution tarball " ++ tarballPath ++ " - rebuild with the following command:"
      putStrLn $ "./gradlew :distribution:archives:" ++ jobName ++ ":assemble"
      return tarballPath
    _ -> do
      putStrLn $ "multiple tarballs found matching " ++ tarballLocation ++ "/" ++ prefix ++ "*" ++ suffix ++ ": " ++ show dirContents
      exitWith $ ExitFailure 1

resolveTarball NoTarget = do
  tarballDir <- getEnv "ES_TARBALL_DIR"
  allEntries <- listDirectory tarballDir
  putStrLn $ "Available versions in $ES_TARBALL_DIR=" ++ tarballDir
  mapM_ putStrLn $ sort
        [ reverse $ drop (length suffix) $ reverse $ drop (length prefix) entry
        | entry <- allEntries
        , prefix `isPrefixOf` entry
        , suffix `isSuffixOf` entry
        ]
  exitWith ExitSuccess
  where
    prefix = "elasticsearch-"
    suffix = ".tar.gz"

unpackAfresh :: Config -> IO FilePath
unpackAfresh config = do
  tarballPath <- resolveTarball $ cTarget config

  maybePlatform <- lookupEnv "ES_TARBALL_PLATFORM"

  unpackRoot <- getCurrentDirectory
  let unpackPathWithPlatform = unpackRoot </> dropTgzExtension (takeFileName tarballPath)
      unpackPath = case maybePlatform of
        Just platform
          | ("-" ++ platform) `isSuffixOf` unpackPathWithPlatform
          -> take (length unpackPathWithPlatform - length platform - 1) unpackPathWithPlatform
        _ -> unpackPathWithPlatform
  unpackPathExists <- doesDirectoryExist unpackPath
  when unpackPathExists $ do
    putStrLn $ "Directory '" ++ unpackPath ++ "' already exists"
    exitWith $ ExitFailure 2

  callProcess "tar" ["xf", tarballPath]

  unpackPathCreated <- doesDirectoryExist unpackPath
  unless unpackPathCreated $ do
    putStrLn $ "Directory '" ++ unpackPath ++ "' not created"
    exitWith $ ExitFailure 3

  return unpackPath

getVersion :: Target -> Maybe (Int, Int)
getVersion t = do
  TargetVersion v                           <- Just t
  (majorVersionString, _:afterMajorVersion) <- Just $ break (== '.') v
  majorVersion                              <- readMaybe majorVersionString
  minorVersion                              <- readMaybe $ takeWhile (/= '.') afterMajorVersion
  return (majorVersion, minorVersion)

data SetupCommand = SetupCommand String Value

linesFromCommand :: SetupCommand -> [String]
linesFromCommand (SetupCommand uri Null) = [uri, ""]
linesFromCommand (SetupCommand uri v) = [uri, T.unpack $ T.decodeUtf8 $ encode $ v, ""]

main :: IO ()
main = do
  config <- getConfig
  unpackPath <- unpackAfresh config

  let defaultConfigDir = unpackPath </> "config-default"
  renameDirectory (unpackPath </> "config") defaultConfigDir

  let nodes = nodesFromConfig config
      version@(majorVersion,_) = fromMaybe (8, 0) $ getVersion $ cTarget config
      isSecured = cSecured config && 6 <= majorVersion
      useNodeRoles = (7, 10) <= version
      disableGeoIp = (7, 14) <= version && not (cWithGeoIp config)
      setJvmOptions = (7, 10) <= version -- tbc not sure of version

  repoPath <- if cWithRepo config
      then let p = unpackPath </> "repo" in createDirectory p >> return (Just p)
      else return Nothing

  startCommands <- forM nodes $ \n -> do
    let configDir = unpackPath </> ("config-" ++ show (nodeIndex n))
        dataDir   = unpackPath </> ("data-"   ++ show (nodeIndex n))
        runElasticsearch = unpackPath </> "bin" </> "elasticsearch"
    callProcess "cp" ["-R", defaultConfigDir, configDir]
    when setJvmOptions $ writeFile (configDir </> "jvm.options.d" </> "00-es-unpack.options") $ unlines $
      [ "-Xmx1g"
      , "-Xms1g"
      ]
    when (majorVersion >= 7) $ appendFile (configDir </> "unicast_hosts.txt") $ unlines $ nodeUnicastHosts n
    appendFile (configDir </> "elasticsearch.yml") $ unlines $
      [ "node.name: node-"                     ++ show (nodeIndex n)
      , "path.data: " ++ if
        | majorVersion < 8 -> unpackPath </> "data-" ++ show (nodeIndex n)
        | otherwise        ->                "data-" ++ show (nodeIndex n)
      , "network.host: 127.0.0.1"
      , "http.port: "                                ++ show (nodeHttpPort n)
      , (if | majorVersion < 7 -> "transport.tcp.port: "
            | otherwise        -> "transport.port: "
        ) ++ show (nodeTransportPort n)
      ] ++ if
        | useNodeRoles ->
          [ "node.roles: "
          , (if nodeIsMaster   n then "  " else "# ") ++ "- master"
          , (if nodeIsDataNode n then "  " else "# ") ++ "- data"
          , "  - ingest"
          , "  - ml"
          , "  - remote_cluster_client"
          , "  - transform"
          ]
        | otherwise ->
          [ "node.master: " ++ (if nodeIsMaster   n then "true" else "false")
          , "node.data: "   ++ (if nodeIsDataNode n then "true" else "false")
          ]
      ++ if
        | majorVersion <= 6 ->
          [ "discovery.zen.minimum_master_nodes: " ++ show (nodeMinimumMasterNodes n)
          , "discovery.zen.ping.unicast.hosts: "   ++ show (nodeUnicastHosts n)
          ]
        | otherwise ->
          [ "discovery.seed_hosts: []"
          , "discovery.seed_providers: file"
          ]
      ++ [ "path.repo: " ++ p | Just p <- [repoPath] ]
      ++ (if disableGeoIp then ["ingest.geoip.downloader.enabled: false"] else [])
      ++ (if
        | isSecured ->
          [ "xpack.security.enabled: true"
          , "xpack.security.transport.ssl.enabled: true"
          , "xpack.security.transport.ssl.verification_mode: full"
          , "xpack.security.transport.ssl.keystore.path: certs/elastic-certificates.p12"
          , "xpack.security.transport.ssl.truststore.path: certs/elastic-certificates.p12"
          , "xpack.security.http.ssl.enabled: true"
          , "xpack.security.http.ssl.keystore.path: certs/elastic-certificates.p12"
          , "xpack.security.http.ssl.truststore.path: certs/elastic-certificates.p12"
          ]
        | version < (7,14) ->
          []
        | otherwise ->
          [ "xpack.security.enabled: false"
          ])
      ++ (if cEnableDiskWatermarks config
          then []
          else [ "cluster.routing.allocation.disk.watermark.low: 1B"
               , "cluster.routing.allocation.disk.watermark.high: 1B"
               , "cluster.routing.allocation.disk.watermark.flood_stage: 1B"
               ]
         )
      ++ cExtraSettings config

    let nstr = show (nodeIndex n)
        startScreenWith cmd = "screen -t node-" ++ nstr ++ " " ++ nstr ++ " bash -c \"" ++ cmd ++ "\""

    return $ startScreenWith $ if
      | majorVersion <  6 -> "ES_JVM_OPTIONS=" ++ configDir ++ "/jvm.options "
                                ++ runElasticsearch ++ " -Epath.conf=" ++ configDir
      | majorVersion == 6 -> "ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch
      | nodeIndex n  /= 0 || not (cWithBootstrap config)
                          -> "JAVA_HOME= ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch
      | otherwise         -> "JAVA_HOME= ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch ++ " -Ecluster.initial_master_nodes="
           ++ intercalate ","
                  [ "node-" ++ show (nodeIndex n')
                  | n' <- nodes
                  , nodeIsMaster n'
                  ]

  when isSecured $ withCurrentDirectory unpackPath $ do
    setEnv "ES_PATH_CONF" "config-0"
    unsetEnv "JAVA_HOME"
    callProcess "bin/elasticsearch-certutil" ["ca", "--out", "elastic-stack-ca.p12", "--pass", "", "--silent"]
    forM_ nodes $ \n -> do
      let configDir = "config-" ++ show (nodeIndex n)
      setEnv "ES_PATH_CONF" configDir
      createDirectory $ configDir </> "certs"
      callProcess "bin/elasticsearch-certutil" [ "cert"
                                               , "--ca", "elastic-stack-ca.p12", "--ca-pass", ""
                                               , "--ip", "127.0.0.1"
                                               , "--dns", "localhost"
                                               , "--out", configDir </> "certs" </> "elastic-certificates.p12"
                                               , "--pass", ""
                                               , "--silent"]
      callProcess "openssl" ["pkcs12", "-in", "elastic-stack-ca.p12", "-passin", "pass:", "-nokeys", "-out", "elastic-stack-ca.pem"]

  let screenRcPath = unpackPath </> "es-unpack.screenrc"
  writeFile screenRcPath $ unlines $
      [ "sessionname " ++ takeFileName unpackPath ]
      ++ startCommands
      ++ replicate (length nodes - 1) "split"
      ++ concat [ ["select " ++ show (nodeIndex n), "focus down"] | 1 < length nodes, n <- nodes ]

  putStrLn $ "screen -c " ++ screenRcPath

  when isSecured $ do
      putStrLn $ "JAVA_HOME= ES_PATH_CONF=" ++ (unpackPath </> "config-0") ++ " " ++ (unpackPath </> "bin" </> "elasticsearch-setup-passwords") ++ " auto -b"

  let setupPath = unpackPath </> "setup-commands.txt"
  writeFile setupPath $ unlines $ concatMap linesFromCommand
      $  [ SetupCommand "GET /_cluster/health?wait_for_status=green&timeout=1h" Null ]
      ++ [ SetupCommand "PUT /_snapshot/default-repo" $ object
              [ "type" .= String "fs"
              , "settings" .= object
                [ "location" .= p
                ]
              ] | Just p <- [repoPath] ]
  if isSecured
    then putStrLn $ "escli --server https://localhost:" ++ (show $ nodeHttpPort $ head nodes)
            ++ " --certificate-store " ++ (unpackPath </> "elastic-stack-ca.pem")
            ++ " --username elastic --password $ES_UNPACK_PASS"
            ++ " < " ++ setupPath
    else putStrLn $ "escli --server http://localhost:"  ++ (show $ nodeHttpPort $ head nodes) ++ " < " ++ setupPath
