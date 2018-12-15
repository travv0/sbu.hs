module Lib
  ( handleOptions
  )
where

import qualified Data.ByteString               as BS
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           System.Directory
import           System.FilePath
import           System.IO

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
    Right c -> handleCommand command c >>= writeConfig path
    Left  _ -> do
      c <- defaultConfig
      printCreatedConfigMsg path c
      createDirectoryIfMissing True $ takeDirectory path
      handleCommand command c >>= writeConfig path

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

handleCommand :: Command -> Config -> IO Config
handleCommand (AddCmd (AddOptions games)  ) config = addGames config games
handleCommand (ListCmd                    ) config = listGames config
handleCommand (InfoCmd (InfoOptions games)) config = infoGames config games
handleCommand command config =
  error
    $  "Command not yet implemented.  Some potentially useful info:\n"
    ++ show command
    ++ "\n"
    ++ show config

addGames :: Config -> [String] -> IO Config
addGames config games = do
  newGames <- sequence (map promptAddGame games)
  return $ config { configGames = newGames `union` configGames config }

listGames :: Config -> IO Config
listGames config = do
  putStrLn $ intercalate "\n" $ gameNames config
  return config

infoGames :: Config -> [String] -> IO Config
infoGames config games = do
  if null games
    then mapM_ (infoGame config) $ gameNames config
    else mapM_ (infoGame config) games
  return config

promptAddGame :: String -> IO Game
promptAddGame name = do
  putStr $ "Enter save path for " ++ name ++ ": "
  hFlush stdout
  path <- getLine
  if null path
    then promptAddGame name
    else do
      putStr
        "Enter pattern to match files/folders on for backup (leave blank to backup everything in save path): "
      hFlush stdout
      glob <- getLine
      return $ Game name path $ if null glob then "*" else glob

infoGame :: Config -> String -> IO ()
infoGame config gName = do
  let matchingGames = filter (\g -> gameName g == gName) $ configGames config
  if null matchingGames
    then putStrLn $ "No game matching the name " ++ gName
    else do
      let game = head matchingGames
      putStrLn
        $  "Game name: "
        ++ gameName game
        ++ "\n"
        ++ "Game save path: "
        ++ gamePath game
        ++ "\n"
        ++ "Game save glob: "
        ++ gameGlob game
        ++ "\n"

gameNames :: Config -> [String]
gameNames config = sort $ map gameName $ configGames config

