{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Lib.Internal where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, foldM, forM_, unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, catchIOError, finally, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ListM (sortByM)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (elemIndex, intercalate, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Serialize (decode, encode)
import Data.Time (
    UTCTime (utctDay, utctDayTime),
    defaultTimeLocale,
    diffUTCTime,
    formatTime,
    getCurrentTime,
    getCurrentTimeZone,
    secondsToDiffTime,
    utcToLocalTime,
 )
import Options
import Pipes (Pipe, await, for, runEffect, yield, (>->))
import qualified Pipes.Prelude as P
import System.Directory (
    canonicalizePath,
    copyFileWithMetadata,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getAppUserDataDirectory,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
    removeFile,
    renameDirectory,
    renameFile,
 )
import System.Exit (exitFailure)
import System.FilePath (
    addTrailingPathSeparator,
    dropFileName,
    isRelative,
    takeDirectory,
    takeFileName,
    (<.>),
    (</>),
 )
import System.FilePath.Glob (compile, globDir1, matchDefault, matchDotsImplicitly, matchWith)
import System.IO (hPutStrLn, stderr)
import Types

defaultGlob :: String
defaultGlob = "**/*"

defaultConfigDir :: IO FilePath
defaultConfigDir = getAppUserDataDirectory "sbu"

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
    locked <- doesFileExist lockPath
    if locked
        then do
            hPutStrLn stderr $
                "sbu appears to be already running.  If it's not, delete the file `"
                    <> lockPath
                    <> "' and try again"
            liftIO exitFailure
        else do
            createDirectoryIfMissing True $ dropFileName lockPath
            BS.writeFile lockPath BS.empty

deleteLockFile :: IO ()
deleteLockFile = do
    lockPath <- lockFilePath
    locked <- doesFileExist lockPath
    when locked $ removeFile lockPath

withLockFile :: (MonadIO m, MonadMask m) => m () -> m ()
withLockFile f = do
    liftIO createLockFile
    f `finally` liftIO deleteLockFile

defaultConfig :: IO Config
defaultConfig = do
    home <- getHomeDirectory
    return $ Config (home </> "sbu_backups") 15 20 []

printAndLog :: MonadIO m => String -> m ()
printAndLog s = do
    logsDir <- liftIO defaultLogsDir
    liftIO $ createDirectoryIfMissing True logsDir
    now <- liftIO getCurrentTime
    result <- liftIO $ try $ logStr logsDir s now
    case result of
        Left e -> liftIO $ logStr logsDir ("Error: " <> show (e :: IOError)) now
        _ -> return ()
  where
    logStr logsDir str now = do
        hPutStrLn stderr str
        appendFile
            (logsDir </> show (utctDay now) <.> "log")
            ( unlines $
                map ((formatTime defaultTimeLocale "%X" now <> ": ") <>) $
                    lines str
            )

runSbu :: Sbu -> Config -> IO (Maybe Config)
runSbu = runReaderT

createDefaultConfig :: FilePath -> IO Config
createDefaultConfig path = do
    c <- defaultConfig
    printCreatedConfigMsg path c
    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile path $ encode c
    return c

printCreatedConfigMsg :: FilePath -> Config -> IO ()
printCreatedConfigMsg path config = do
    hPutStrLn stderr $
        "Creating new config file at `" <> path <> "'.\n"
            <> "Use the `config' command to update default values, which are:\n"
    printOutput $ printConfig config Nothing Nothing Nothing

readConfig :: FilePath -> IO (Either String Config)
readConfig path = do
    configExists <- doesFileExist path
    if configExists
        then do
            config <- BS.readFile path
            return $ decode config
        else return $ Left $ "Error opening file at `" <> path <> "'."

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = withLockFile $ do
    configExists <- doesFileExist path
    when configExists $ renameFile path $ path <.> "bak"
    BS.writeFile path $ encode config

maybeWriteConfig :: FilePath -> Maybe Config -> IO ()
maybeWriteConfig path config = forM_ config (writeConfig path)

printOutput :: MonadIO m => Logger m r -> m r
printOutput p = runEffect $ for p (liftIO . hPutStrLn stderr)

printAndLogOutput :: MonadIO m => Logger m r -> m r
printAndLogOutput p = runEffect $ for p printAndLog

handleCommand :: Command -> Sbu
handleCommand (AddCmd (AddOptions game savePath glob)) = do
    path <- liftIO $ canonicalizePath' savePath
    printOutput $ addGame game path glob
handleCommand ListCmd = printOutput listGames
handleCommand (InfoCmd (InfoOptions games)) = printOutput $ infoGames games
handleCommand (RemoveCmd (RemoveOptions games yes)) =
    printOutput $ P.repeatM (liftIO getLine) >-> removeGames yes games
handleCommand (EditCmd (EditOptions game mNewName mNewPath mNewGlob)) =
    printOutput $ editGame game mNewName mNewPath mNewGlob
handleCommand (ConfigCmd (ConfigOptions mBackupDir mBackupFreq mBackupsToKeep)) = do
    config <- ask
    newBackupDir <- liftIO $ canonicalizePath' $ fromMaybe (configBackupDir config) mBackupDir
    printOutput $ editConfig newBackupDir mBackupFreq mBackupsToKeep
handleCommand (ConfigCmd ConfigDefaults) = do
    dc <- liftIO defaultConfig
    printOutput $
        editConfig
            (configBackupDir dc)
            (Just $ configBackupFreq dc)
            (Just $ configBackupsToKeep dc)
handleCommand (BackupCmd (BackupOptions games loop verbose)) =
    printAndLogOutput $ backupGames loop verbose games

validGameNameChars :: [Char]
validGameNameChars = ['A' .. 'z'] <> ['0' .. '9'] <> ['-', '_']

isValidGameName :: String -> Bool
isValidGameName = all (`elem` validGameNameChars)

addGame ::
    MonadReader Config m =>
    String ->
    FilePath ->
    Maybe String ->
    Logger m (Maybe Config)
addGame game path glob = do
    config <- ask
    if
            | game `elem` map gameName (configGames config) -> do
                yield $ "Error: Game with the name " <> game <> " already exists"
                return Nothing
            | not $ isValidGameName game -> do
                yield $
                    "Error: Invalid characters in name `" <> game
                        <> "': only alphanumeric characters, underscores, and hyphens are allowed"
                return Nothing
            | otherwise -> do
                if isRelative path
                    then do
                        yield $
                            "Error: Save path must be absolute, but relative path was supplied: "
                                <> path
                        return Nothing
                    else do
                        let newGlob = case fromMaybe "" glob of
                                "none" -> ""
                                g -> g
                            newGame = Game game path newGlob
                        yield "Game added successfully:\n"
                        printGame newGame Nothing Nothing Nothing
                        return $ Just $ config{configGames = newGame : configGames config}

canonicalizePath' :: FilePath -> IO FilePath
canonicalizePath' ('~' : path) = do
    let trimmedPath = dropWhile (\c -> c == '/' || c == '\\') path
    homeDir <- getHomeDirectory
    canonicalizePath $ homeDir </> trimmedPath
canonicalizePath' path = canonicalizePath path

listGames :: MonadReader Config m => Logger m (Maybe Config)
listGames = do
    gNames <- gameNames
    yield $ intercalate "\n" gNames
    return Nothing

removeGames :: MonadReader Config m => Bool -> [String] -> Pipe String String m (Maybe Config)
removeGames yes games = do
    config <- ask
    if yes
        then do
            yield $ "Removed the following games:\n" <> intercalate "\n" games
            return $
                Just $
                    config
                        { configGames =
                            filter ((`notElem` games) . gameName) $
                                configGames config
                        }
        else do
            warnMissingGames games
            gamesToRemove <-
                filterM promptRemove $
                    filter ((`elem` games) . gameName) $
                        configGames config
            mapM_ (\g -> yield $ "Removed " <> gameName g) gamesToRemove
            return $
                Just $
                    config
                        { configGames = filter ((`notElem` map gameName gamesToRemove) . gameName) $ configGames config
                        }

infoGames :: MonadReader Config m => [String] -> Logger m (Maybe Config)
infoGames games = do
    allGameNames <- gameNames
    mapM_ infoGame $ if null games then allGameNames else games
    return Nothing

editGame ::
    (MonadIO m, MonadReader Config m) =>
    String ->
    Maybe String ->
    Maybe FilePath ->
    Maybe String ->
    Logger m (Maybe Config)
editGame gName mNewName mNewPath mNewGlob = do
    config <- ask
    mGame <- getGameByName gName
    case (mGame, mNewName, mNewPath, mNewGlob) of
        (Nothing, _, _, _) -> do
            yield $
                "Error: Game with the name "
                    <> gName
                    <> " doesn't exist"
            return Nothing
        (_, Nothing, Nothing, Nothing) -> do
            yield "One or more of --name, --path, or --glob must be provided."
            return Nothing
        (Just g, _, _, _) -> do
            let i = elemIndex (gameName g) $ map gameName (configGames config)
                mSplitList = splitAt <$> i <*> pure (configGames config)
            case mSplitList of
                Nothing -> do
                    warnMissingGames [gName]
                    return Nothing
                Just (_, []) -> error "Couldn't find game in list"
                Just (front, game : back) -> do
                    let newName = fromMaybe (gameName game) mNewName
                        newGlob = case fromMaybe (gameGlob game) mNewGlob of
                            "none" -> ""
                            glob -> glob
                    newPath <- liftIO $ canonicalizePath' $ fromMaybe (gamePath game) mNewPath
                    let editedGame =
                            game
                                { gameName = newName
                                , gamePath = newPath
                                , gameGlob = newGlob
                                }
                    if
                            | isRelative newPath -> do
                                yield $
                                    "Error: Save path must be absolute, but relative path was supplied: "
                                        <> newPath
                                return Nothing
                            | not $ isValidGameName newName -> do
                                yield $
                                    "Error: Invalid characters in name `" <> newName
                                        <> "': only alphanumeric characters, `_', `-', and `/' are allowed"
                                return Nothing
                            | otherwise -> do
                                printGame game (Just newName) (Just newPath) (Just newGlob)
                                backupDirExists <-
                                    liftIO $ doesDirectoryExist $ configBackupDir config </> gName
                                when (isJust mNewName && backupDirExists) $ do
                                    yield "Game name changed, renaming backup directory..."
                                    liftIO $
                                        renameDirectory
                                            (configBackupDir config </> gName)
                                            (configBackupDir config </> newName)

                                return $ Just $ config{configGames = front <> (editedGame : back)}

printConfigRow :: Functor m => String -> String -> Maybe String -> Logger m ()
printConfigRow label val newVal =
    yield $
        label <> ": " <> val
            <> case newVal of
                Just nv
                    | val == nv -> ""
                    | otherwise -> " -> " <> nv
                Nothing -> ""

printConfig ::
    Functor m =>
    Config ->
    Maybe String ->
    Maybe Integer ->
    Maybe Integer ->
    Logger m ()
printConfig config mNewBackupDir mNewBackupFreq mNewBackupsToKeep = do
    printConfigRow "Backup path" (configBackupDir config) mNewBackupDir
    printConfigRow
        "Backup frequency (in minutes)"
        (show $ configBackupFreq config)
        (show <$> mNewBackupFreq)
    printConfigRow
        "Number of backups to keep"
        (show $ configBackupsToKeep config)
        (show <$> mNewBackupsToKeep)
    yield ""

editConfig ::
    MonadReader Config m =>
    FilePath ->
    Maybe Integer ->
    Maybe Integer ->
    Logger m (Maybe Config)
editConfig newBackupDir mBackupFreq mBackupsToKeep = do
    config <- ask
    let newBackupFreq = fromMaybe (configBackupFreq config) mBackupFreq
        newBackupsToKeep = fromMaybe (configBackupsToKeep config) mBackupsToKeep

    if isRelative newBackupDir
        then do
            yield $
                "Error: Backup path must be absolute, but relative path was supplied: "
                    <> newBackupDir
            return Nothing
        else do
            printConfig config (Just newBackupDir) mBackupFreq mBackupsToKeep
            return $
                Just $
                    config
                        { configBackupDir = newBackupDir
                        , configBackupFreq = newBackupFreq
                        , configBackupsToKeep = newBackupsToKeep
                        }

backupGames ::
    (MonadIO m, MonadReader Config m, MonadCatch m) =>
    Bool ->
    Bool ->
    [String] ->
    Logger m (Maybe Config)
backupGames loop verbose games = do
    config <- ask
    allGameNames <- gameNames
    let gamesToBackup = if null games then allGameNames else games
    warnings <-
        foldM
            ( \acc game -> do
                warnings <-
                    backupGame game `catchIOError` \e -> do
                        yield $ "Error backing up " <> game <> ": " <> show e
                        return []
                return $ acc <> warnings
            )
            []
            gamesToBackup
    unless (null warnings) $ do
        yield $
            show (length warnings) <> " warning"
                <> (if length warnings == 1 then "" else "s")
                <> " occurred:"
        if verbose
            then do
                yield ""
                mapM_ yield warnings
            else yield "Pass --verbose flag to print all warnings after backup completes\n"
    if loop
        then do
            liftIO $ threadDelay $ fromIntegral $ configBackupFreq config * 60 * 1000000
            backupGames verbose loop games
        else return Nothing

backupGame ::
    (MonadIO m, MonadReader Config m, MonadCatch m) =>
    String ->
    Logger m [String]
backupGame gName = do
    config <- ask
    startTime <- liftIO getCurrentTime
    mGame <- getGameByName gName
    case mGame of
        Just game -> do
            isDirectory <- liftIO $ doesDirectoryExist $ gamePath game
            if isDirectory
                then do
                    (backedUpCount, warnings) <-
                        backupFiles
                            (gameName game)
                            (gamePath game)
                            (gameGlob game)
                            (gamePath game)
                            (configBackupDir config </> gName)
                    now <- liftIO getCurrentTime
                    tz <- liftIO getCurrentTimeZone
                    when (backedUpCount > 0) $
                        yield $
                            "Finished backing up "
                                <> show backedUpCount
                                <> " file"
                                <> (if backedUpCount == 1 then "" else "s")
                                <> ( if null warnings
                                        then ""
                                        else
                                            " with " <> show (length warnings) <> " warning"
                                                <> (if length warnings == 1 then "" else "s")
                                   )
                                <> " for "
                                <> gName
                                <> " in "
                                <> show (diffUTCTime now startTime)
                                <> " on "
                                <> formatTime defaultTimeLocale "%c" (utcToLocalTime tz now)
                                <> "\n"
                    return warnings
                else do
                    yield $
                        "Warning: Path set for "
                            <> gName
                            <> " doesn't exist: "
                            <> gamePath game
                    return []
        Nothing -> do
            warnMissingGames [gName]
            return []

backupFiles ::
    (MonadIO m, MonadReader Config m, MonadCatch m) =>
    String ->
    FilePath ->
    String ->
    FilePath ->
    FilePath ->
    Logger m (Integer, [String])
backupFiles game basePath glob from to = do
    files <- liftIO $ listDirectory from
    foldM
        ( \(c, es) f -> do
            (newCount, newErrs) <- backupFile game basePath glob (from </> f) (to </> f)
            return (c + newCount, es <> newErrs)
        )
        (0, [])
        files

backupFile ::
    (MonadIO m, MonadReader Config m, MonadCatch m) =>
    String ->
    FilePath ->
    String ->
    FilePath ->
    FilePath ->
    Logger m (Integer, [String])
backupFile game basePath glob from to = do
    isDirectory <- liftIO $ doesDirectoryExist from
    if isDirectory
        then backupFiles game basePath glob from to
        else if globMatches then backupFile' else return (0, [])
  where
    globMatches =
        matchWith
            (matchDefault{matchDotsImplicitly = True})
            ( compile $
                addTrailingPathSeparator basePath
                    <> if null glob
                        then defaultGlob
                        else glob
            )
            from
    backupFile' =
        do
            backupExists <- liftIO $ doesFileExist to
            fromModTime <- liftIO $ getModificationTime from
            mToModTime <-
                if backupExists
                    then liftIO $ Just <$> getModificationTime to
                    else return Nothing
            case mToModTime of
                Just toModTime ->
                    if fromModTime
                        { utctDayTime =
                            secondsToDiffTime $ round $ utctDayTime fromModTime
                        }
                        /= toModTime
                            { utctDayTime =
                                secondsToDiffTime $ round $ utctDayTime toModTime
                            }
                        then do
                            liftIO $
                                renameFile to $
                                    to <.> "bak" <.> formatModifiedTime toModTime
                            copyAndCleanup
                        else return (0, [])
                Nothing -> copyAndCleanup
            `catchIOError` \e -> do
                let warning =
                        "Unable to backup file " <> to <> " for game " <> game <> ":\n"
                            <> show e
                            <> "\n"
                yield $ "Warning: " <> warning
                return (1, [warning])
    copyAndCleanup = do
        liftIO $ createDirectoryIfMissing True $ dropFileName to
        yield $ from <> " ==>\n    " <> to
        liftIO $ copyFileWithMetadata from to
        cleanupBackups to
        return (1, [])

cleanupBackups :: (MonadIO m, MonadReader Config m) => FilePath -> Logger m ()
cleanupBackups backupPath = do
    config <- ask
    when (configBackupsToKeep config > 0) $ do
        files <-
            liftIO $
                (backupPath :)
                    <$> globDir1
                        ( compile $
                            takeFileName backupPath
                                <> ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
                        )
                        (dropFileName backupPath)
        when (toInteger (length files) > configBackupsToKeep config) $ do
            sortedFiles <-
                liftIO $
                    sortByM
                        ( \f1 f2 -> do
                            modTime1 <- getModificationTime f1
                            modTime2 <- getModificationTime f2
                            return $ modTime2 `compare` modTime1
                        )
                        files
            let filesToDelete =
                    drop (fromIntegral $ configBackupsToKeep config) sortedFiles
            mapM_
                ( \f -> do
                    yield $ "Deleting " <> f
                    liftIO $ removeFile f
                )
                filesToDelete

formatModifiedTime :: UTCTime -> String
formatModifiedTime = formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S"

infoGame :: MonadReader Config m => String -> Logger m ()
infoGame gName = do
    config <- ask
    let matchingGames = filter (\g -> gameName g == gName) $ configGames config
    case matchingGames of
        [] -> warnMissingGames [gName]
        game : _ -> printGame game Nothing Nothing Nothing

printGame ::
    Functor m =>
    Game ->
    Maybe String ->
    Maybe FilePath ->
    Maybe String ->
    Logger m ()
printGame game mNewName mNewPath mNewGlob = do
    printConfigRow "Name" (gameName game) mNewName
    printConfigRow "Save path" (gamePath game) mNewPath
    when (not (null (gameGlob game)) || isJust mNewGlob) $
        printConfigRow "Save glob" (gameGlob game) mNewGlob
    yield ""

gameNames :: MonadReader Config m => m [String]
gameNames = asks $ sort . map gameName . configGames

promptRemove :: Functor m => Game -> Pipe String String m Bool
promptRemove game = do
    yield $ "Permanently delete " <> gameName game <> "? (y/N) "
    input <- await
    return $ toLower (head $ if null input then "n" else input) == 'y'

warnMissingGames :: (MonadReader Config m, Foldable t) => t String -> Logger m ()
warnMissingGames games = do
    config <- ask
    mapM_
        ( \g ->
            when (g `notElem` map gameName (configGames config)) $
                yield $
                    "Warning: No game named `"
                        <> g
                        <> "'"
        )
        games

getGameByName :: MonadReader Config m => String -> m (Maybe Game)
getGameByName name = do
    config <- ask
    case filter (\g -> gameName g == name) $ configGames config of
        [] -> return Nothing
        (game : _) -> return $ Just game
