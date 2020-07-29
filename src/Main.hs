{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Process
import Data.List
import Data.Aeson
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

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

  unicastHosts = [ "127.0.0.1:" ++ show (nodeTransportPort n)
                 | n <- result
                 , nodeIsMaster n
                 ]

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
    let suffix = case maybePlatform of
          Just platform | 7 <= majorVersionFromTarget target -> "-" ++ platform
          _                                                  -> ""

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

  unpackRoot <- getCurrentDirectory
  let unpackPath = unpackRoot </> dropTgzExtension (takeFileName tarballPath)
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

majorVersionFromTarget :: Target -> Int
majorVersionFromTarget (TargetVersion v) = read (takeWhile (/= '.') v)
majorVersionFromTarget _                 = 8

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
      majorVersion = majorVersionFromTarget $ cTarget config
      isSecured = cSecured config && 6 <= majorVersion

  repoPath <- if cWithRepo config
      then let p = unpackPath </> "repo" in createDirectory p >> return (Just p)
      else return Nothing

  startCommands <- forM nodes $ \n -> do
    let configDir = unpackPath </> ("config-" ++ show (nodeIndex n))
        dataDir   = unpackPath </> ("data-"   ++ show (nodeIndex n))
        runElasticsearch = unpackPath </> "bin" </> "elasticsearch"
    callProcess "cp" ["-R", defaultConfigDir, configDir]
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
      , "node.master: " ++ (if nodeIsMaster   n then "true" else "false")
      , "node.data: "   ++ (if nodeIsDataNode n then "true" else "false")
      ] ++ if
        | majorVersion <= 6 ->
          [ "discovery.zen.minimum_master_nodes: " ++ show (nodeMinimumMasterNodes n)
          , "discovery.zen.ping.unicast.hosts: "   ++ show (nodeUnicastHosts n)
          ]
        | otherwise ->
          [ "discovery.seed_hosts: "               ++ show (nodeUnicastHosts n)
          ]
      ++ [ "path.repo: " ++ p | Just p <- [repoPath] ]
      ++ (if isSecured
          then
          [ "xpack.security.enabled: true"
          , "xpack.security.transport.ssl.enabled: true"
          , "xpack.security.transport.ssl.verification_mode: full"
          , "xpack.security.transport.ssl.keystore.path: certs/elastic-certificates.p12"
          , "xpack.security.transport.ssl.truststore.path: certs/elastic-certificates.p12"
          , "xpack.security.http.ssl.enabled: true"
          , "xpack.security.http.ssl.keystore.path: certs/elastic-certificates.p12"
          , "xpack.security.http.ssl.truststore.path: certs/elastic-certificates.p12"
          ]
          else [])
      ++ cExtraSettings config

    let nstr = show (nodeIndex n)
        startScreenWith cmd = "screen -t node-" ++ nstr ++ " " ++ nstr ++ " bash -c \"" ++ cmd ++ "\""

    return $ startScreenWith $ if
      | majorVersion <  6 -> "ES_JVM_OPTIONS=" ++ configDir ++ "/jvm.options "
                                ++ runElasticsearch ++ " -Epath.conf=" ++ configDir
      | majorVersion == 6 -> "ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch
      | nodeIndex n  /= 0 -> "ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch
      | otherwise         -> "ES_PATH_CONF=" ++ configDir ++ " " ++ runElasticsearch ++ " -Ecluster.initial_master_nodes="
           ++ intercalate ","
                  [ "node-" ++ show (nodeIndex n')
                  | n' <- nodes
                  , nodeIsMaster n'
                  ]

  when isSecured $ withCurrentDirectory unpackPath $ do
    setEnv "ES_PATH_CONF" "config-0"
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

  let setupPath = unpackPath </> "setup-commands.txt"
  writeFile setupPath $ unlines $ concatMap linesFromCommand
      $  [ SetupCommand "GET /_cluster/health?wait_for_status=green&timeout=1h" Null ]
      ++ [ SetupCommand "PUT /_snapshot/default-repo" $ object
              [ "type" .= String "fs"
              , "settings" .= object
                [ "location" .= p
                ]
              ] | Just p <- [repoPath] ]
  putStrLn $ "escli --server http://localhost:" ++ (show $ nodeHttpPort $ head nodes) ++ " < " ++ setupPath
