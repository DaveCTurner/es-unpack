{-# LANGUAGE MultiWayIf #-}

module Main where

import Config
import Control.Monad
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Process
import Data.List

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
    , nodeHttpPort           = 9200 + i
    , nodeTransportPort      = 9300 + i
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
resolveTarball (TargetVersion version) = do
  tarballDir <- getEnv "ES_TARBALL_DIR"
  let tarballPath = tarballDir </> ("elasticsearch-" ++ version ++ ".tar.gz")

  tarballExists <- doesFileExist tarballPath
  unless tarballExists $ do
    putStrLn $ "Tarball " ++ tarballPath ++ " not found. Download with:"

    let urlPrefix
          | "-SNAPSHOT" `isSuffixOf` version = "https://snapshots.elastic.co/downloads/elasticsearch/elasticsearch-"
          | otherwise                        = "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-"

    putStrLn $ "curl -f " ++ urlPrefix ++ version ++ ".tar.gz -o '" ++ tarballPath
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

  let unpackPath = dropTgzExtension $ takeFileName tarballPath
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

main :: IO ()
main = do
  config <- getConfig
  unpackPath <- unpackAfresh config

  let defaultConfigDir = unpackPath </> "config-default"
  renameDirectory (unpackPath </> "config") defaultConfigDir

  let nodes = nodesFromConfig config

  startCommands <- forM nodes $ \n -> do
    let configDir = unpackPath </> ("config-" ++ show (nodeIndex n))
        dataDir   = unpackPath </> ("data-"   ++ show (nodeIndex n))
        runElasticsearch = unpackPath </> "bin" </> "elasticsearch"
        majorVersion = case cTarget config of
          TargetVersion v -> read (takeWhile (/= '.') v)
          _               -> 8
    callProcess "cp" ["-R", defaultConfigDir, configDir]
    appendFile (configDir </> "elasticsearch.yml") $ unlines $
      [ "node.name: node-"                     ++ show (nodeIndex n)
      , "path.data: "++ if
        | majorVersion < 6 -> unpackPath </> "data-" ++ show (nodeIndex n)
        | otherwise        ->                "data-" ++ show (nodeIndex n)
      , "network.host: 127.0.0.1"
      , "http.port: "                          ++ show (nodeHttpPort n)
      , "transport.tcp.port: "                 ++ show (nodeTransportPort n)
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

  let screenRcPath = unpackPath </> "es-unpack.screenrc"
  writeFile screenRcPath $ unlines $
      [ "sessionname " ++ unpackPath ]
      ++ startCommands
      ++ replicate (length nodes - 1) "split"
      ++ concat [ ["select " ++ show (nodeIndex n), "focus down"] | n <- nodes ]

  putStrLn $ "screen -c " ++ screenRcPath
