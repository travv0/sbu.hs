module Lib
  ( handleOptions
  )
where

import           Control.Monad
import qualified Data.ByteString               as BS
import           Data.Char
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
      let backupPath = path <.> "bak"
      backupExists <- doesFileExist backupPath
      if backupExists
        then do
          putStrLn
            $ "Error reading config file, attempting to read from backup..."
          backupConfig <- BS.readFile backupPath
          case decode backupConfig of
            Right c -> handleCommand command c >>= writeConfig path
            Left  _ -> do
              c <- defaultConfig
              createDefaultConfig c path
              handleCommand command c >>= writeConfig path
        else do
          c <- defaultConfig
          createDefaultConfig c path
          handleCommand command c >>= writeConfig path

createDefaultConfig :: Config -> FilePath -> IO ()
createDefaultConfig config path = do
  printCreatedConfigMsg path config
  createDirectoryIfMissing True $ takeDirectory path

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
    else return $ Left $ "Error opening file at `" ++ path ++ "'."

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = do
  configExists <- doesFileExist path
  when (configExists) $ renameFile path $ path <.> "bak"
  BS.writeFile path $ encode config

handleCommand :: Command -> Config -> IO Config
handleCommand (AddCmd (AddOptions games))   config = addGames config games
handleCommand ListCmd                       config = listGames config
handleCommand (InfoCmd (InfoOptions games)) config = infoGames config games
handleCommand (RemoveCmd (RemoveOptions games yes)) config =
  removeGames config yes games
handleCommand (EditCmd (EditOptions game mNewName mNewPath mNewGlob)) config =
  editGame config game mNewName mNewPath mNewGlob
handleCommand (ConfigCmd (ConfigOptions mBackupDir mBackupFreq mBackupsToKeep)) config
  = editConfig config mBackupDir mBackupFreq mBackupsToKeep
handleCommand (BackupCmd (BackupOptions games loop)) config =
  backupGames config loop games
handleCommand command config =
  error
    $  "Command not yet implemented.  Some potentially useful info:\n"
    ++ show command
    ++ "\n"
    ++ show config

addGames :: Config -> [String] -> IO Config
addGames config games = do
  newGames <- map fromJust . filter isJust <$> mapM (promptAddGame config) games
  return $ config { configGames = newGames `union` configGames config }

listGames :: Config -> IO Config
listGames config = do
  putStrLn $ intercalate "\n" $ gameNames config
  return config

removeGames :: Config -> Bool -> [String] -> IO Config
removeGames config yes games = if yes
  then return $ config
    { configGames = filter ((`elem` games) . gameName) $ configGames config
    }
  else do
    warnMissingGames config games
    gamesToRemove <-
      filterM promptRemove $ filter ((`elem` games) . gameName) $ configGames
        config
    return $ config
      { configGames = filter (`notElem` gamesToRemove) $ configGames config
      }

infoGames :: Config -> [String] -> IO Config
infoGames config games = do
  if null games
    then mapM_ (infoGame config) $ gameNames config
    else mapM_ (infoGame config) games
  return config

editGame
  :: Config
  -> String
  -> Maybe String
  -> Maybe FilePath
  -> Maybe String
  -> IO Config
editGame config gName mNewName mNewPath mNewGlob =
  if all isNothing [mNewName, mNewPath, mNewGlob]
    then do
      putStrLn "One or more of --name, --path, or --glob must be provided."
      return config
    else do
      let i          = elemIndex gName (map gameName $ configGames config)
          mSplitList = splitAt <$> i <*> pure (configGames config)
      case mSplitList of
        Nothing -> do
          warnMissingGames config [gName]
          return config
        Just (front, game : back) -> do
          let
            newName    = fromMaybe (gameName game) mNewName
            newPath    = fromMaybe (gamePath game) mNewPath
            newGlob    = fromMaybe (gameGlob game) mNewGlob
            editedGame = game { gameName = newName
                              , gamePath = newPath
                              , gameGlob = newGlob
                              }
          when (isJust mNewName)
            $  putStrLn
            $  "Name: "
            ++ gName
            ++ " -> "
            ++ newName
          when (isJust mNewPath)
            $  putStrLn
            $  "Save path: "
            ++ gamePath game
            ++ " -> "
            ++ newPath
          when (isJust mNewGlob)
            $  putStrLn
            $  "Save glob: "
            ++ gameGlob game
            ++ " -> "
            ++ newGlob
          return config { configGames = front ++ (editedGame : back) }

editConfig
  :: Config -> Maybe FilePath -> Maybe Integer -> Maybe Integer -> IO Config
editConfig config mBackupDir mBackupFreq mBackupsToKeep = do
  let newBackupDir     = fromMaybe (configBackupDir config) mBackupDir
      newBackupFreq    = fromMaybe (configBackupFreq config) mBackupFreq
      newBackupsToKeep = fromMaybe (configBackupsToKeep config) mBackupsToKeep
  putStrLn $ "Backup path: " ++ configBackupDir config ++ if isJust mBackupDir
    then " -> " ++ newBackupDir
    else ""
  putStrLn
    $  "Backup frequency (in minutes): "
    ++ show (configBackupFreq config)
    ++ if isJust mBackupFreq then " -> " ++ show newBackupFreq else ""
  putStrLn
    $  "Number of backups to keep: "
    ++ show (configBackupsToKeep config)
    ++ if isJust mBackupsToKeep then " -> " ++ show newBackupsToKeep else ""
  return config { configBackupDir     = newBackupDir
                , configBackupFreq    = newBackupFreq
                , configBackupsToKeep = newBackupsToKeep
                }

backupGames :: Config -> Bool -> [String] -> IO Config
backupGames config loop games = undefined

promptAddGame :: Config -> String -> IO (Maybe Game)
promptAddGame config name = do
  if name `elem` map gameName (configGames config)
    then do
      putStrLn $ "Warning: Game with the name " ++ name ++ " already exists"
      return Nothing
    else do
      putStr $ "Enter save path for " ++ name ++ ": "
      hFlush stdout
      path <- getLine
      if null path
        then promptAddGame config name
        else do
          putStr
            "Enter pattern to match files/folders on for backup (leave blank to backup everything in save path): "
          hFlush stdout
          glob <- getLine
          putStrLn ""
          return $ Just $ Game name path glob

infoGame :: Config -> String -> IO ()
infoGame config gName = do
  let matchingGames = filter (\g -> gameName g == gName) $ configGames config
  if null matchingGames
    then putStrLn $ "No game matching the name " ++ gName
    else do
      let game = head matchingGames
      putStrLn
        $  "Name: "
        ++ gameName game
        ++ "\n"
        ++ "Save path: "
        ++ gamePath game
        ++ "\n"
        ++ if null (gameGlob game)
             then ""
             else "Save glob: " ++ gameGlob game ++ "\n"

gameNames :: Config -> [String]
gameNames config = sort $ map gameName $ configGames config

promptRemove :: Game -> IO Bool
promptRemove game = do
  putStr $ "Permanently delete " ++ gameName game ++ "? (y/N) "
  hFlush stdout
  input <- getLine
  return $ toLower (head $ if null input then "n" else input) == 'y'

warnMissingGames :: Config -> [String] -> IO ()
warnMissingGames config games = mapM_
  (\g ->
    when (g `notElem` map gameName (configGames config))
      $  putStrLn
      $  "Warning: No game named `"
      ++ g
      ++ "'"
  )
  games
