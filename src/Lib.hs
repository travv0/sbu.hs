module Lib
  ( handleOptions
  )
where

import qualified Data.ByteString               as BS
import           Data.Maybe
import           Data.Serialize
import           System.Directory
import           System.FilePath

import           Options
import           Types

defaultConfigPath :: IO FilePath
defaultConfigPath = do
  home <- getHomeDirectory
  return $ home </> ".sbu/config"

defaultConfig :: IO Config
defaultConfig = do
  home <- getHomeDirectory
  return $ Config (home </> "sbu_backups") 15 20 []

handleOptions :: SbuOptions -> IO ()
handleOptions (SbuOptions configPath command) = do
  path   <- fromMaybe <$> defaultConfigPath <*> pure configPath
  config <- readConfig path
  case config of
    Right c -> handleCommand command c
    Left  _ -> do
      c <- defaultConfig
      printCreatedConfigMsg path c
      writeConfig path c
      handleCommand command c

printCreatedConfigMsg :: FilePath -> Config -> IO ()
printCreatedConfigMsg path config =
  putStrLn
    $  "Creating new config file at `"
    ++ path
    ++ "'.\n"
    ++ "Use the `config' command to update default values, which are:\n"
    ++ "Backup directory: "
    ++ configBackupDir config
    ++ "\n"
    ++ "Backup frequency: "
    ++ show (configBackupFreq config)
    ++ "\n"
    ++ "Number of backups to keep: "
    ++ show (configBackupsToKeep config)
    ++ "\n"

handleCommand :: Command -> Config -> IO ()
handleCommand (AddCmd (AddOptions games)) _ = print games
handleCommand c conf = print $ show c ++ " " ++ show conf

readConfig :: FilePath -> IO (Either String Config)
readConfig path = do
  configExists <- doesFileExist path
  if configExists
    then do
      config <- BS.readFile path
      return $ decode config
    else return $ Left $ "Error optining file at `" ++ path ++ "'."

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = BS.writeFile path $ encode config
