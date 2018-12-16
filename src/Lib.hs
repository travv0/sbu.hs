module Lib
  ( handleOptions
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad
import           Control.Monad.ListM            ( sortByM )
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.Time
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob           ( match
                                                , compile
                                                , globDir1
                                                )
import           System.IO

import           Options
import           Types

defaultGlob :: String
defaultGlob = "**/*"

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
            "Error reading config file, attempting to read from backup..."
          backupConfig <- BS.readFile backupPath
          case decode backupConfig of
            Right c -> handleCommand command c >>= writeConfig path
            Left  _ -> handleWithNewConfig command path
        else handleWithNewConfig command path

handleWithNewConfig :: Command -> FilePath -> IO ()
handleWithNewConfig command path = do
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
  when configExists $ renameFile path $ path <.> "bak"
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
handleCommand (ConfigCmd ConfigDefaults) config = do
  dc <- defaultConfig
  editConfig config
             (Just $ configBackupDir dc)
             (Just $ configBackupFreq dc)
             (Just $ configBackupsToKeep dc)
handleCommand (BackupCmd (BackupOptions games loop)) config =
  backupGames config loop games

addGames :: Config -> [String] -> IO Config
addGames config games = do
  newGames <- catMaybes <$> mapM (promptAddGame config) games
  return $ config { configGames = newGames `union` configGames config }

listGames :: Config -> IO Config
listGames config = do
  putStrLn $ intercalate "\n" $ gameNames config
  return config

removeGames :: Config -> Bool -> [String] -> IO Config
removeGames config yes games = if yes
  then do
    putStrLn $ "Removed the following games:\n" ++ intercalate "\n" games
    return $ config
      { configGames = filter ((`notElem` games) . gameName) $ configGames config
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
        Just (_    , []         ) -> error "Couldn't find game in list"
        Just (front, game : back) -> do
          let
            newName    = fromMaybe (gameName game) mNewName
            newPath    = fromMaybe (gamePath game) mNewPath
            newGlob'   = fromMaybe (gameGlob game) mNewGlob
            newGlob    = if null newGlob' then defaultGlob else newGlob'
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

          backupDirExists <-
            doesDirectoryExist $ configBackupDir config </> gName
          when (isJust mNewName && backupDirExists) $ do
            putStrLn "Game name changed, renaming backup directory..."
            renameDirectory (configBackupDir config </> gName)
                            (configBackupDir config </> newName)

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
backupGames config loop games = do
  mapM_ (backupGame config) $ if null games then gameNames config else games
  if loop
    then do
      threadDelay $ fromIntegral $ configBackupFreq config * 60 * 1000000
      backupGames config loop games
    else return config

backupGame :: Config -> String -> IO ()
backupGame config gName = do
  startTime <- getCurrentTime
  case getGameByName config gName of
    Just game -> do
      isDirectory <- doesDirectoryExist $ gamePath game
      if isDirectory
        then do
          anyBackedUp <- backupFiles config
                                     (gamePath game)
                                     (gameGlob game)
                                     (gamePath game)
                                     (configBackupDir config </> gName)
          now <- getCurrentTime
          tz  <- getCurrentTimeZone
          when anyBackedUp
            $  putStrLn
            $  "Finished backing up "
            ++ gName
            ++ " in "
            ++ show (diffUTCTime now startTime)
            ++ " on "
            ++ formatTime defaultTimeLocale "%c" (utcToLocalTime tz now)
            ++ "\n"
        else
          putStrLn
          $  "Warning: Path set for "
          ++ gName
          ++ " doesn't exist: "
          ++ gamePath game
    Nothing -> warnMissingGames config [gName]

backupFiles :: Config -> FilePath -> String -> FilePath -> FilePath -> IO Bool
backupFiles config basePath glob from to = do
  files <- getDirectoryContents from
  or <$> mapM (\f -> backupFile config basePath glob (from </> f) (to </> f))
              (filter (\f -> f /= "." && f /= "..") files)

backupFile :: Config -> FilePath -> String -> FilePath -> FilePath -> IO Bool
backupFile config basePath glob from to = do
  isDirectory <- doesDirectoryExist from
  if isDirectory
    then backupFiles config basePath glob from to
    else do
      backupExists <- doesFileExist to
      fromModTime  <- getModificationTime from
      mToModTime   <- if backupExists
        then Just <$> getModificationTime to
        else return Nothing
      case mToModTime of
        Just toModTime -> if fromModTime /= toModTime
          then do
            renameFile to $ to <.> "bak" <.> formatModifiedTime toModTime
            copyAndCleanup
          else return False
        Nothing -> copyAndCleanup
 where
  copyAndCleanup =
    if match (compile $ addTrailingPathSeparator basePath ++ glob) from
      then do
        createDirectoryIfMissing True $ dropFileName to
        putStrLn $ from ++ " ==>\n\t\t" ++ to
        copyFileWithMetadata from to
        cleanupBackups config to
        return True
      else return False

cleanupBackups :: Config -> FilePath -> IO ()
cleanupBackups config backupPath = when (configBackupsToKeep config > 0) $ do
  files <- (backupPath :) <$> globDir1
    (compile
    $ takeFileName backupPath
    ++ ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
    )
    (dropFileName backupPath)
  when (toInteger (length files) > configBackupsToKeep config) $ do
    sortedFiles <- sortByM
      (\f1 f2 -> do
        modTime1 <- getModificationTime f1
        modTime2 <- getModificationTime f2
        return $ modTime2 `compare` modTime1
      )
      files
    let filesToDelete =
          drop (fromIntegral $ configBackupsToKeep config) sortedFiles
    mapM_
      (\f -> do
        putStrLn $ "Deleting " ++ f
        removeFile f
      )
      filesToDelete

formatModifiedTime :: UTCTime -> String
formatModifiedTime = formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S"

promptAddGame :: Config -> String -> IO (Maybe Game)
promptAddGame config name = case getGameByName config name of
  Just _ -> do
    putStrLn $ "Warning: Game with the name " ++ name ++ " already exists"
    return Nothing
  Nothing -> do
    putStr $ "Enter save path for " ++ name ++ ": "
    hFlush stdout
    path <- getLine
    if null path
      then promptAddGame config name
      else do
        putStr
          "Enter glob pattern to match files/folders on for backup (leave blank to backup everything): "
        hFlush stdout
        glob <- getLine
        putStrLn ""
        return $ Just $ Game name path $ if null glob then defaultGlob else glob

infoGame :: Config -> String -> IO ()
infoGame config gName = do
  let matchingGames = filter (\g -> gameName g == gName) $ configGames config
  if null matchingGames
    then warnMissingGames config [gName]
    else do
      let game = head matchingGames
      putStrLn
        $  "Name: "
        ++ gameName game
        ++ "\n"
        ++ "Save path: "
        ++ gamePath game
        ++ "\n"
        ++ if gameGlob game == defaultGlob
             then ""
             else "Save glob: " ++ gameGlob game ++ "\n"

gameNames :: Config -> [String]
gameNames config = sort $ map gameName $ configGames config

promptRemove :: Game -> IO Bool
promptRemove game = do
  putStr $ "Permanently delete " ++ gameName game ++ "? (y/N) "
  hFlush stdout
  input <- getLine
  let rm = toLower (head $ if null input then "n" else input) == 'y'
  when rm $ putStrLn $ "Removed " ++ gameName game
  return rm

warnMissingGames :: Config -> [String] -> IO ()
warnMissingGames config = mapM_
  (\g ->
    when (g `notElem` map gameName (configGames config))
      $  putStrLn
      $  "Warning: No game named `"
      ++ g
      ++ "'"
  )

getGameByName :: Config -> String -> Maybe Game
getGameByName config name =
  case filter (\g -> gameName g == name) $ configGames config of
    []         -> Nothing
    (game : _) -> Just game
