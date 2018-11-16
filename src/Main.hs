module Main where

import Config
import Control.Monad
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Process

data Node = Node
  { nodeIndex         :: Int
  , nodeHttpPort      :: Int
  , nodeTransportPort :: Int
  , nodeIsMaster      :: Bool
  , nodeIsDataNode    :: Bool
  , nodeUnicastHosts  :: [String]
  , nodeMinimumMasterNodes :: Int
  } deriving (Show, Eq)

nodes :: Config -> [Node]
nodes config = result

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

unpackAfresh :: Config -> IO FilePath
unpackAfresh config = do

  tarballPath <- case cVersionOrTarball config of
    Left version -> do
      tarballDir <- getEnv "ES_TARBALL_DIR"
      return $ tarballDir </> ("elasticsearch-" ++ version ++ ".tar.gz")
    Right tbp -> return tbp

  tarballExists <- doesFileExist tarballPath
  unless tarballExists $ do
    case cVersionOrTarball config of
      Left version -> do
        putStrLn $ "Tarball " ++ tarballPath ++ " not found. Download with:"
        putStrLn $ "curl https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-" 
          ++ version ++ ".tar.gz -o '" ++ tarballPath ++ ".partial' && mv -v '"
          ++ tarballPath ++ ".partial' '" ++ tarballPath ++ "'"
      Right tarballPath -> do
        putStrLn $ "Tarball " ++ tarballPath ++ " not found."
    exitWith $ ExitFailure 1

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

  forM_ (nodes config) $ \n -> do
    let configDir = unpackPath </> ("config-" ++ show (nodeIndex n))
        dataDir   = unpackPath </> ("data-"   ++ show (nodeIndex n))
    callProcess "cp" ["-R", defaultConfigDir, configDir]
    appendFile (configDir </> "elasticsearch.yml") $ unlines
      [ "node.name: node-"                     ++ show (nodeIndex n)
      , "path.data: data-"                     ++ show (nodeIndex n)
      , "network.host: 127.0.0.1"
      , "http.port: "                          ++ show (nodeHttpPort n)
      , "transport.tcp.port: "                 ++ show (nodeTransportPort n)
      , "discovery.zen.ping.unicast.hosts: "   ++ show (nodeUnicastHosts n)
      , "discovery.zen.minimum_master_nodes: " ++ show (nodeMinimumMasterNodes n)
      , "node.master: " ++ (if nodeIsMaster   n then "true" else "false")
      , "node.data: "   ++ (if nodeIsDataNode n then "true" else "false")
      ]
    putStrLn $ "ES_PATH_CONF=" ++ configDir ++ " " ++ (unpackPath </> "bin" </> "elasticsearch")
