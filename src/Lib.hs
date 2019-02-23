{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Lib
  ( handleOptions
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( try
                                                , throwIO
                                                )
import           Control.Monad
import           Control.Monad.IO.Class
import           UnliftIO                       ( MonadUnliftIO
                                                , finally
                                                )
import           Control.Monad.ListM            ( sortByM )
import           Control.Monad.Reader
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.Time
import           Foreign.C.Error                ( Errno(Errno)
                                                , ePIPE
                                                )
import qualified GHC.IO.Exception              as G
import           Pipes
import           System.Directory
import           System.Exit                    ( exitFailure )
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

defaultConfigDir :: IO FilePath
defaultConfigDir = do
  home <- getHomeDirectory
  return $ home </> ".sbu/"

defaultConfigPath :: IO FilePath
defaultConfigPath = do
  configDir <- defaultConfigDir
  return $ configDir </> "config"

defaultLogsDir :: IO FilePath
defaultLogsDir = do
  configDir <- defaultConfigDir
  return $ configDir </> "logs"

lockFilePath :: IO FilePath
lockFilePath = do
  configDir <- defaultConfigDir
  return $ configDir </> ".lock"

createLockFile :: IO ()
createLockFile = do
  lockPath <- lockFilePath
  locked   <- doesFileExist lockPath
  if locked
    then do
      putStrLn
        $  "sbu appears to be already running.  If it's not, delete the file `"
        ++ lockPath
        ++ "' and try again"
      liftIO exitFailure
    else BS.writeFile lockPath BS.empty

deleteLockFile :: IO ()
deleteLockFile = do
  lockPath <- lockFilePath
  locked   <- doesFileExist lockPath
  when locked $ removeFile lockPath

withLockFile :: (MonadUnliftIO m) => m () -> m ()
withLockFile f = do
  liftIO createLockFile
  f `finally` liftIO deleteLockFile

defaultConfig :: IO Config
defaultConfig = do
  home <- getHomeDirectory
  return $ Config (home </> "sbu_backups") 15 20 []

stdoutAndLog :: MonadIO m => Consumer' String m ()
stdoutAndLog = do
  logsDir <- liftIO defaultLogsDir
  liftIO $ createDirectoryIfMissing True logsDir
  go logsDir
 where
  go logsDir = do
    now <- liftIO getCurrentTime
    str <- await
    x   <- liftIO $ try $ do
      putStrLn str
      appendFile
        (logsDir </> show (utctDay now) <.> "log")
        ( unlines
        $ map ((formatTime defaultTimeLocale "%X" now ++ ": ") ++)
        $ lines str
        )
    case x of
      Left G.IOError { G.ioe_type = G.ResourceVanished, G.ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
      Left  e  -> liftIO (throwIO e)
      Right () -> go logsDir

runSbu :: Sbu -> Config -> IO ()
runSbu sbu = runReaderT $ runEffect (sbu >-> stdoutAndLog)

handleOptions :: SbuOptions -> IO ()
handleOptions (SbuOptions configPath command) = do
  path    <- fromMaybe <$> defaultConfigPath <*> pure configPath
  econfig <- readConfig path
  config  <- case econfig of
    Right c -> return c
    Left  _ -> do
      let backupPath = path <.> "bak"
      backupExists <- doesFileExist backupPath
      if backupExists
        then do
          putStrLn
            "Error reading config file, attempting to read from backup..."
          backupConfig <- BS.readFile backupPath
          case decode backupConfig of
            Right c -> return c
            Left  _ -> createDefaultConfig path
        else createDefaultConfig path
  _ <- runSbu (handleCommand command path) config
  return ()

createDefaultConfig :: FilePath -> IO Config
createDefaultConfig path = do
  c <- defaultConfig
  printCreatedConfigMsg path c
  createDirectoryIfMissing True $ takeDirectory path
  BS.writeFile path $ encode c
  return c

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

maybeWriteConfig :: FilePath -> Maybe Config -> IO ()
maybeWriteConfig path config = forM_ config (writeConfig path)

handleCommand :: Command -> FilePath -> Sbu

handleCommand (AddCmd (AddOptions games)) path = do
  config <- addGames games
  liftIO $ withLockFile $ maybeWriteConfig path config

handleCommand ListCmd                               _    = listGames

handleCommand (InfoCmd   (InfoOptions games      )) _    = infoGames games

handleCommand (RemoveCmd (RemoveOptions games yes)) path = do
  config <- removeGames yes games
  liftIO $ withLockFile $ maybeWriteConfig path config

handleCommand (EditCmd (EditOptions game mNewName mNewPath mNewGlob)) path = do
  config <- editGame game mNewName mNewPath mNewGlob
  liftIO $ withLockFile $ maybeWriteConfig path config

handleCommand (ConfigCmd (ConfigOptions mBackupDir mBackupFreq mBackupsToKeep)) path
  = do
    config <- editConfig mBackupDir mBackupFreq mBackupsToKeep
    liftIO $ withLockFile $ maybeWriteConfig path config

handleCommand (ConfigCmd ConfigDefaults) path = do
  dc     <- liftIO defaultConfig
  config <- editConfig (Just $ configBackupDir dc)
                       (Just $ configBackupFreq dc)
                       (Just $ configBackupsToKeep dc)
  liftIO $ withLockFile $ maybeWriteConfig path config

handleCommand (BackupCmd (BackupOptions games loop)) _ = backupGames loop games

addGames :: (MonadIO m, MonadReader Config m) => [String] -> m (Maybe Config)
addGames games = do
  config   <- ask
  newGames <- catMaybes <$> mapM promptAddGame games
  return $ Just $ config { configGames = newGames `union` configGames config }

listGames :: (MonadIO m, MonadReader Config m) => m ()
listGames = do
  gNames <- gameNames
  liftIO $ putStrLn $ intercalate "\n" gNames

removeGames
  :: (MonadIO m, MonadReader Config m) => Bool -> [String] -> m (Maybe Config)
removeGames yes games = do
  config <- ask
  if yes
    then do
      liftIO
        $  putStrLn
        $  "Removed the following games:\n"
        ++ intercalate "\n" games
      return $ Just $ config
        { configGames = filter ((`notElem` games) . gameName)
                          $ configGames config
        }
    else do
      warnMissingGames games
      gamesToRemove <-
        liftIO
        $ filterM promptRemove
        $ filter ((`elem` games) . gameName)
        $ configGames config
      mapM_ (\g -> liftIO $ putStrLn $ "Removed " ++ gameName g) gamesToRemove
      return $ Just $ config
        { configGames = filter (`notElem` gamesToRemove) $ configGames config
        }

infoGames :: (MonadIO m, MonadReader Config m) => [String] -> m ()
infoGames games = do
  gNames <- gameNames
  if null games then mapM_ infoGame gNames else mapM_ infoGame games

editGame
  :: (MonadIO m, MonadReader Config m)
  => String
  -> Maybe String
  -> Maybe FilePath
  -> Maybe String
  -> m (Maybe Config)
editGame gName mNewName mNewPath mNewGlob = do
  config <- ask
  mgName <- getGameByName gName
  if all isNothing [mNewName, mNewPath, mNewGlob]
    then do
      liftIO $ putStrLn
        "One or more of --name, --path, or --glob must be provided."
      return Nothing
    else case mgName of
      Nothing -> do
        liftIO
          $  putStrLn
          $  "Error: Game with the name "
          ++ gName
          ++ " doesn't exist"
        return Nothing
      Just g -> do
        let i          = elemIndex g (configGames config)
            mSplitList = splitAt <$> i <*> pure (configGames config)
        case mSplitList of
          Nothing -> do
            warnMissingGames [gName]
            return Nothing
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
            if isRelative newPath
              then do
                liftIO
                  $ putStrLn
                  $ "Error: Save path must be absolute, but relative path was supplied: "
                  ++ newPath
                return Nothing
              else do
                when (isJust mNewName)
                  $  liftIO
                  $  putStrLn
                  $  "Name: "
                  ++ gName
                  ++ " -> "
                  ++ newName
                when (isJust mNewPath)
                  $  liftIO
                  $  putStrLn
                  $  "Save path: "
                  ++ gamePath game
                  ++ " -> "
                  ++ newPath
                when (isJust mNewGlob)
                  $  liftIO
                  $  putStrLn
                  $  "Save glob: "
                  ++ gameGlob game
                  ++ " -> "
                  ++ newGlob

                backupDirExists <-
                  liftIO $ doesDirectoryExist $ configBackupDir config </> gName
                when (isJust mNewName && backupDirExists) $ do
                  liftIO $ putStrLn
                    "Game name changed, renaming backup directory..."
                  liftIO $ renameDirectory
                    (configBackupDir config </> gName)
                    (configBackupDir config </> newName)

                return $ Just $ config
                  { configGames = front ++ (editedGame : back)
                  }

editConfig
  :: (MonadIO m, MonadReader Config m)
  => Maybe FilePath
  -> Maybe Integer
  -> Maybe Integer
  -> m (Maybe Config)
editConfig mBackupDir mBackupFreq mBackupsToKeep = do
  config <- ask
  let newBackupDir     = fromMaybe (configBackupDir config) mBackupDir
      newBackupFreq    = fromMaybe (configBackupFreq config) mBackupFreq
      newBackupsToKeep = fromMaybe (configBackupsToKeep config) mBackupsToKeep

  if isRelative newBackupDir
    then do
      liftIO
        $ putStrLn
        $ "Error: Backup path must be absolute, but relative path was supplied: "
        ++ newBackupDir
      return Nothing
    else do
      liftIO
        $  putStrLn
        $  "Backup path: "
        ++ configBackupDir config
        ++ if isJust mBackupDir then " -> " ++ newBackupDir else ""
      liftIO
        $  putStrLn
        $  "Backup frequency (in minutes): "
        ++ show (configBackupFreq config)
        ++ if isJust mBackupFreq then " -> " ++ show newBackupFreq else ""
      liftIO
        $  putStrLn
        $  "Number of backups to keep: "
        ++ show (configBackupsToKeep config)
        ++ if isJust mBackupsToKeep then " -> " ++ show newBackupsToKeep else ""
      return $ Just $ config { configBackupDir     = newBackupDir
                             , configBackupFreq    = newBackupFreq
                             , configBackupsToKeep = newBackupsToKeep
                             }

backupGames
  :: (MonadIO m, MonadReader Config m) => Bool -> [String] -> Logger m ()
backupGames loop games = do
  config <- ask
  gNames <- gameNames
  mapM_ backupGame $ if null games then gNames else games
  when loop $ do
    liftIO $ threadDelay $ fromIntegral $ configBackupFreq config * 60 * 1000000
    backupGames loop games

backupGame :: (MonadIO m, MonadReader Config m) => String -> Logger m ()
backupGame gName = do
  config    <- ask
  startTime <- liftIO getCurrentTime
  mgName    <- getGameByName gName
  case mgName of
    Just game -> do
      isDirectory <- liftIO $ doesDirectoryExist $ gamePath game
      if isDirectory
        then do
          anyBackedUp <- backupFiles (gamePath game)
                                     (gameGlob game)
                                     (gamePath game)
                                     (configBackupDir config </> gName)
          now <- liftIO getCurrentTime
          tz  <- liftIO getCurrentTimeZone
          when anyBackedUp
            $  yield
            $  "Finished backing up "
            ++ gName
            ++ " in "
            ++ show (diffUTCTime now startTime)
            ++ " on "
            ++ formatTime defaultTimeLocale "%c" (utcToLocalTime tz now)
            ++ "\n"
        else
          yield
          $  "Warning: Path set for "
          ++ gName
          ++ " doesn't exist: "
          ++ gamePath game
    Nothing -> warnMissingGames [gName]

backupFiles
  :: (MonadIO m, MonadReader Config m)
  => FilePath
  -> String
  -> FilePath
  -> FilePath
  -> Logger m Bool
backupFiles basePath glob from to = do
  files <- liftIO $ getDirectoryContents from
  or <$> mapM (\f -> backupFile basePath glob (from </> f) (to </> f))
              (filter (\f -> f /= "." && f /= "..") files)

backupFile
  :: (MonadIO m, MonadReader Config m)
  => FilePath
  -> String
  -> FilePath
  -> FilePath
  -> Logger m Bool
backupFile basePath glob from to = do
  isDirectory <- liftIO $ doesDirectoryExist from
  if isDirectory
    then backupFiles basePath glob from to
    else do
      backupExists <- liftIO $ doesFileExist to
      fromModTime  <- liftIO $ getModificationTime from
      mToModTime   <- if backupExists
        then liftIO $ Just <$> getModificationTime to
        else return Nothing
      case mToModTime of
        Just toModTime ->
          if fromModTime
                 { utctDayTime = secondsToDiffTime $ round $ utctDayTime
                                   fromModTime
                 }
               /= toModTime
                    { utctDayTime = secondsToDiffTime $ round $ utctDayTime
                                      toModTime
                    }
            then do
              liftIO
                $   renameFile to
                $   to
                <.> "bak"
                <.> formatModifiedTime toModTime
              copyAndCleanup
            else return False
        Nothing -> copyAndCleanup
 where
  copyAndCleanup =
    if match (compile $ addTrailingPathSeparator basePath ++ glob) from
      then do
        liftIO $ createDirectoryIfMissing True $ dropFileName to
        yield $ from ++ " ==>\n\t\t" ++ to
        liftIO $ copyFileWithMetadata from to
        cleanupBackups to
        return True
      else return False

cleanupBackups :: (MonadIO m, MonadReader Config m) => FilePath -> Logger m ()
cleanupBackups backupPath = do
  config <- ask
  when (configBackupsToKeep config > 0) $ do
    files <- liftIO $ (backupPath :) <$> globDir1
      (compile
      $ takeFileName backupPath
      ++ ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
      )
      (dropFileName backupPath)
    when (toInteger (length files) > configBackupsToKeep config) $ do
      sortedFiles <- liftIO $ sortByM
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
          yield $ "Deleting " ++ f
          liftIO $ removeFile f
        )
        filesToDelete

formatModifiedTime :: UTCTime -> String
formatModifiedTime = formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S"

promptAddGame :: (MonadIO m, MonadReader Config m) => String -> m (Maybe Game)
promptAddGame name = do
  mgame <- getGameByName name
  case mgame of
    Just _ -> do
      liftIO
        $  putStrLn
        $  "Error: Game with the name "
        ++ name
        ++ " already exists"
      return Nothing
    Nothing -> do
      liftIO $ putStr $ "Enter save path for " ++ name ++ ": "
      liftIO $ hFlush stdout
      path <- liftIO getLine
      if
        | null path -> promptAddGame name
        | isRelative path -> do
          liftIO $ putStrLn
            "Warning: Path must be absolute, but relative path was supplied."
          promptAddGame name
        | otherwise -> do
          liftIO
            $ putStr
                "Enter glob pattern to match files/folders on for backup (leave blank to backup everything): "
          liftIO $ hFlush stdout
          glob <- liftIO getLine
          liftIO $ putStrLn ""
          return $ Just $ Game name path $ if null glob
            then defaultGlob
            else glob

infoGame :: (MonadIO m, MonadReader Config m) => String -> m ()
infoGame gName = do
  config <- ask
  let matchingGames = filter (\g -> gameName g == gName) $ configGames config
  if null matchingGames
    then warnMissingGames [gName]
    else do
      let game = head matchingGames
      liftIO
        $  putStrLn
        $  "Name: "
        ++ gameName game
        ++ "\n"
        ++ "Save path: "
        ++ gamePath game
        ++ "\n"
        ++ if gameGlob game == defaultGlob
             then ""
             else "Save glob: " ++ gameGlob game ++ "\n"

gameNames :: (MonadIO m, MonadReader Config m) => m [String]
gameNames = asks $ sort . map gameName . configGames

promptRemove :: Game -> IO Bool
promptRemove game = do
  putStr $ "Permanently delete " ++ gameName game ++ "? (y/N) "
  hFlush stdout
  input <- getLine
  return $ toLower (head $ if null input then "n" else input) == 'y'

warnMissingGames :: (MonadIO m, MonadReader Config m) => [String] -> m ()
warnMissingGames games = do
  config <- ask
  liftIO $ mapM_
    (\g ->
      when (g `notElem` map gameName (configGames config))
        $  putStrLn
        $  "Warning: No game named `"
        ++ g
        ++ "'"
    )
    games

getGameByName :: (MonadReader Config m) => String -> m (Maybe Game)
getGameByName name = do
  config <- ask
  case filter (\g -> gameName g == name) $ configGames config of
    []         -> return Nothing
    (game : _) -> return $ Just game
