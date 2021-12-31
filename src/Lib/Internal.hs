{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib.Internal (
    defaultConfigPath,
    readConfig,
    writeConfig,
    createDefaultConfig,
    handleCommand,
    maybeWriteConfig,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.Catch (MonadMask, catchIOError, finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ListM (sortByM)
import Control.Monad.Reader (ask, asks, local)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Foldable (find, toList, traverse_)
import Data.List (elemIndex, intercalate, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, isJust)
import Data.Serialize (decode, encode)
import Data.Set (Set)
import Data.String (IsString)
import qualified Data.Text as T
import Data.Time (
    UTCTime,
    defaultTimeLocale,
    diffUTCTime,
    formatTime,
    getCurrentTime,
    getCurrentTimeZone,
    secondsToDiffTime,
    utcToLocalTime,
    utctDayTime,
 )
import GHC.Exts (fromList)
import Options (
    AddOptions (..),
    BackupOptions (..),
    Command (..),
    ConfigOptions (..),
    EditOptions (..),
    InfoOptions (..),
    RemoveOptions (..),
 )
import Prettyprinter (Doc, Pretty, annotate, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, hPutDoc)
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
    pathIsSymbolicLink,
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
import System.FilePath.Glob (
    compile,
    globDir1,
    matchDefault,
    matchDotsImplicitly,
    matchWith,
 )
import System.IO (Handle, hFlush, hPutStrLn, stderr, stdout)
import Types (Config (..), Game (..), Output (..), RunConfig (..), Sbu)

defaultGlob :: String
defaultGlob = "**/*"

defaultConfigDir :: IO FilePath
defaultConfigDir = getAppUserDataDirectory "sbu"

defaultConfigPath :: IO FilePath
defaultConfigPath = do
    configDir <- defaultConfigDir
    return $ configDir </> "config"

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

colorText :: (Pretty a, Semigroup a) => Color -> a -> Doc AnsiStyle
colorText c = annotate (color c) . pretty

errorText :: (Pretty a, Semigroup a, IsString a) => a -> Doc AnsiStyle
errorText = colorText Red . ("Error: " <>)

warningText :: (Pretty a, Semigroup a, IsString a) => a -> Doc AnsiStyle
warningText = colorText Yellow . ("Warning: " <>)

infoText :: (Pretty a, Semigroup a, IsString a) => a -> Doc AnsiStyle
infoText = colorText Blue . ("Info: " <>)

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
    printConfig config Nothing Nothing Nothing

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
maybeWriteConfig path = traverse_ (writeConfig path)

hPutDocLn :: Handle -> Doc AnsiStyle -> IO ()
hPutDocLn h doc = do
    hPutDoc h $ doc <> "\n"
    hFlush h

printOutput :: MonadIO m => Output -> m ()
printOutput = liftIO . uncurry hPutDocLn . outputToDoc

outputToDoc :: Output -> (Handle, Doc AnsiStyle)
outputToDoc (Normal s) = (stdout, pretty s)
outputToDoc (Info s) = (stderr, infoText $ T.pack s)
outputToDoc (Warning s) = (stderr, warningText $ T.pack s)
outputToDoc (Error s) = (stderr, errorText $ T.pack s)

handleCommand :: Command -> Sbu (Maybe Config)
handleCommand (AddCmd (AddOptions game savePath glob)) = do
    path <- liftIO $ canonicalizePath' savePath
    addGame game path glob
handleCommand ListCmd = listGames
handleCommand (InfoCmd (InfoOptions games)) = infoGames games
handleCommand (RemoveCmd (RemoveOptions games yes)) = removeGames yes games
handleCommand (EditCmd (EditOptions game mNewName mNewPath mNewGlob)) =
    editGame game mNewName mNewPath mNewGlob
handleCommand
    (ConfigCmd (ConfigOptions mBackupDir mBackupFreq mBackupsToKeep)) = do
        config <- asks runConfigConfig
        newBackupDir <-
            liftIO $
                canonicalizePath' $
                    fromMaybe (configBackupDir config) mBackupDir
        editConfig newBackupDir mBackupFreq mBackupsToKeep
handleCommand (ConfigCmd ConfigDefaults) = do
    dc <- liftIO defaultConfig
    editConfig
        (configBackupDir dc)
        (Just $ configBackupFreq dc)
        (Just $ configBackupsToKeep dc)
handleCommand (BackupCmd (BackupOptions games loop verbose)) =
    local (\c -> c{runConfigVerbose = verbose}) $ backupGames loop games

validGameNameChars :: Set Char
validGameNameChars =
    fromList $
        ['A' .. 'Z']
            <> ['a' .. 'z']
            <> ['0' .. '9']
            <> ['-', '_']

isValidGameName :: String -> Bool
isValidGameName = all (`elem` validGameNameChars)

addGame :: String -> FilePath -> Maybe String -> Sbu (Maybe Config)
addGame game path glob = do
    config <- asks runConfigConfig
    if
            | game `elem` map gameName (configGames config) -> do
                printOutput $
                    Error $ "Game with the name " <> game <> " already exists"
                return Nothing
            | not $ isValidGameName game -> do
                printOutput $
                    Error $
                        "Invalid characters in name `" <> game
                            <> "': only alphanumeric characters, underscores, and hyphens are allowed"
                return Nothing
            | otherwise ->
                if isRelative path
                    then do
                        printOutput $
                            Error $
                                "Save path must be absolute, but relative path was supplied: "
                                    <> path
                        return Nothing
                    else do
                        let newGlob = case fromMaybe "" glob of
                                "none" -> ""
                                g -> g
                            newGame = Game game path newGlob
                        printOutput $ Normal "Game added successfully:\n"
                        liftIO $ printGame newGame Nothing Nothing Nothing
                        return $
                            Just $
                                config
                                    { configGames = newGame : configGames config
                                    }

canonicalizePath' :: FilePath -> IO FilePath
canonicalizePath' ('~' : path) = do
    let trimmedPath = dropWhile (\c -> c == '/' || c == '\\') path
    homeDir <- getHomeDirectory
    canonicalizePath $ homeDir </> trimmedPath
canonicalizePath' path = canonicalizePath path

listGames :: Sbu (Maybe Config)
listGames = do
    gNames <- gameNames
    printOutput $ Normal $ intercalate "\n" gNames
    return Nothing

removeGames :: Bool -> NonEmpty String -> Sbu (Maybe Config)
removeGames yes games = do
    config <- asks runConfigConfig
    if yes
        then do
            printOutput $
                Normal $
                    "Removed the following games:\n"
                        <> intercalate "\n" (toList games)
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
            mapM_
                (\g -> printOutput $ Normal $ "Removed " <> gameName g)
                gamesToRemove
            return $
                Just $
                    config
                        { configGames =
                            filter
                                ( (`notElem` map gameName gamesToRemove)
                                    . gameName
                                )
                                $ configGames config
                        }

infoGames :: [String] -> Sbu (Maybe Config)
infoGames games = do
    allGameNames <- gameNames
    mapM_ infoGame $ if null games then allGameNames else games
    return Nothing

editGame ::
    String ->
    Maybe String ->
    Maybe FilePath ->
    Maybe String ->
    Sbu (Maybe Config)
editGame gName mNewName mNewPath mNewGlob = do
    config <- asks runConfigConfig
    case (mNewName, mNewPath, mNewGlob) of
        (Nothing, Nothing, Nothing) -> do
            printOutput $
                Error
                    "One or more of --name, --path, or --glob must be provided."
            return Nothing
        (_, _, _) -> do
            let i = elemIndex gName $ map gameName (configGames config)
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
                    newPath <-
                        liftIO $
                            canonicalizePath' $
                                fromMaybe (gamePath game) mNewPath
                    let editedGame =
                            game
                                { gameName = newName
                                , gamePath = newPath
                                , gameGlob = newGlob
                                }
                    if
                            | isRelative newPath -> do
                                printOutput $
                                    Error $
                                        "Save path must be absolute, but relative path was supplied: "
                                            <> newPath
                                return Nothing
                            | not $ isValidGameName newName -> do
                                printOutput $
                                    Error $
                                        "Invalid characters in name `"
                                            <> newName
                                            <> "': only alphanumeric characters, `_', `-', and `/' are allowed"
                                return Nothing
                            | otherwise -> do
                                liftIO $
                                    printGame
                                        game
                                        (Just newName)
                                        (Just newPath)
                                        (Just newGlob)
                                backupDirExists <-
                                    liftIO $
                                        doesDirectoryExist $
                                            configBackupDir config </> gName
                                when (isJust mNewName && backupDirExists) $ do
                                    printOutput $
                                        Info
                                            "Game name changed, renaming backup directory..."
                                    liftIO $
                                        renameDirectory
                                            (configBackupDir config </> gName)
                                            (configBackupDir config </> newName)

                                return $
                                    Just $
                                        config
                                            { configGames =
                                                front <> (editedGame : back)
                                            }

printConfigRow :: MonadIO m => String -> String -> Maybe String -> m ()
printConfigRow label val newVal =
    printOutput $
        Normal $
            label <> ": " <> val
                <> case newVal of
                    Just nv
                        | val == nv -> ""
                        | otherwise -> " -> " <> nv
                    Nothing -> ""

printConfig ::
    MonadIO m =>
    Config ->
    Maybe String ->
    Maybe Integer ->
    Maybe Integer ->
    m ()
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
    printOutput $ Normal ""

editConfig :: FilePath -> Maybe Integer -> Maybe Integer -> Sbu (Maybe Config)
editConfig newBackupDir mBackupFreq mBackupsToKeep = do
    config <- asks runConfigConfig
    let newBackupFreq = fromMaybe (configBackupFreq config) mBackupFreq
        newBackupsToKeep = fromMaybe (configBackupsToKeep config) mBackupsToKeep

    if isRelative newBackupDir
        then do
            printOutput $
                Error $
                    "Backup path must be absolute, but relative path was supplied: "
                        <> newBackupDir
            return Nothing
        else do
            liftIO $ printConfig config (Just newBackupDir) mBackupFreq mBackupsToKeep
            return $
                Just $
                    config
                        { configBackupDir = newBackupDir
                        , configBackupFreq = newBackupFreq
                        , configBackupsToKeep = newBackupsToKeep
                        }

backupGames :: Bool -> [String] -> Sbu (Maybe Config)
backupGames loop games = do
    RunConfig{runConfigConfig = config, runConfigVerbose = verbose} <- ask
    allGameNames <- gameNames
    let gamesToBackup = if null games then allGameNames else games
    warnings <-
        foldM
            ( \acc game -> do
                warnings <-
                    backupGame game `catchIOError` \e -> do
                        printOutput $
                            Error $
                                "Error backing up " <> game <> ": " <> show e
                        return []
                return $ acc <> warnings
            )
            []
            gamesToBackup
    unless (null warnings) $ do
        let msg =
                show (length warnings) <> " warning"
                    <> (if length warnings == 1 then "" else "s")
                    <> " occurred:"
        if verbose
            then do
                printOutput $ Warning $ msg <> "\n"
                mapM_ (printOutput . Warning) warnings
            else
                printOutput $
                    Warning $
                        msg <> "\nPass --verbose flag to print all warnings after backup completes\n"
    if loop
        then do
            liftIO $
                threadDelay $
                    fromIntegral $ configBackupFreq config * 60 * 1000000
            backupGames loop games
        else return Nothing

backupGame :: String -> Sbu [String]
backupGame gName = do
    config <- asks runConfigConfig
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
                        printOutput $
                            Normal $
                                "\nFinished backing up "
                                    <> show backedUpCount
                                    <> " file"
                                    <> (if backedUpCount == 1 then "" else "s")
                                    <> ( if null warnings
                                            then ""
                                            else
                                                " with "
                                                    <> show (length warnings)
                                                    <> " warning"
                                                    <> ( if length warnings == 1
                                                            then ""
                                                            else "s"
                                                       )
                                       )
                                    <> " for "
                                    <> gName
                                    <> " in "
                                    <> show (diffUTCTime now startTime)
                                    <> " on "
                                    <> formatTime
                                        defaultTimeLocale
                                        "%c"
                                        (utcToLocalTime tz now)
                                    <> "\n"
                    return warnings
                else do
                    printOutput $
                        Warning $
                            "Path set for " <> gName <> " doesn't exist: "
                                <> gamePath game
                    return []
        Nothing -> do
            warnMissingGames [gName]
            return []

backupFiles ::
    String ->
    FilePath ->
    String ->
    FilePath ->
    FilePath ->
    Sbu (Integer, [String])
backupFiles game basePath glob from to = do
    files <- liftIO $ listDirectory from
    foldM
        ( \(c, es) f -> do
            (newCount, newErrs) <-
                backupFile game basePath glob (from </> f) (to </> f)
            return (c + newCount, es <> newErrs)
        )
        (0, [])
        files

backupFile ::
    String ->
    FilePath ->
    String ->
    FilePath ->
    FilePath ->
    Sbu (Integer, [String])
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
            verbose <- asks runConfigVerbose
            isSymlink <- liftIO $ pathIsSymbolicLink from
            if isSymlink
                then do
                    when verbose $
                        printOutput $ Info $ from <> " appears to be a link to somewhere else in the filesystem. Skipping..."
                    return (0, [])
                else do
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
                                    secondsToDiffTime $
                                        round $ utctDayTime fromModTime
                                }
                                /= toModTime
                                    { utctDayTime =
                                        secondsToDiffTime $
                                            round $ utctDayTime toModTime
                                    }
                                then do
                                    liftIO $
                                        renameFile to $
                                            to <.> "bak"
                                                <.> formatModifiedTime toModTime
                                    copyAndCleanup
                                else return (0, [])
                        Nothing -> copyAndCleanup
            `catchIOError` \e -> do
                let warning =
                        "Unable to backup file " <> to <> " for game " <> game
                            <> ":\n"
                            <> show e
                            <> "\n"
                printOutput $ Warning warning
                return (1, [warning])
    copyAndCleanup = do
        liftIO $ createDirectoryIfMissing True $ dropFileName to
        printOutput $ Normal $ from <> " ==>\n    " <> to
        liftIO $ copyFileWithMetadata from to
        cleanupBackups to
        return (1, [])

cleanupBackups :: FilePath -> Sbu ()
cleanupBackups backupPath = do
    RunConfig{runConfigConfig = config, runConfigVerbose = verbose} <- ask
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
                    when verbose $ printOutput $ Info $ "Deleting " <> f
                    liftIO $ removeFile f
                )
                filesToDelete

formatModifiedTime :: UTCTime -> String
formatModifiedTime = formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S"

infoGame :: String -> Sbu ()
infoGame gName = do
    cGames <- asks $ configGames . runConfigConfig
    let matchingGames = filter (\g -> gameName g == gName) cGames
    case matchingGames of
        [] -> warnMissingGames [gName]
        game : _ -> printGame game Nothing Nothing Nothing

printGame ::
    MonadIO m =>
    Game ->
    Maybe String ->
    Maybe FilePath ->
    Maybe String ->
    m ()
printGame game mNewName mNewPath mNewGlob = do
    printConfigRow "Name" (gameName game) mNewName
    printConfigRow "Save path" (gamePath game) mNewPath
    when (not (null (gameGlob game)) || isJust mNewGlob) $
        printConfigRow "Save glob" (gameGlob game) mNewGlob
    printOutput $ Normal ""

gameNames :: Sbu [String]
gameNames = asks $ sort . map gameName . configGames . runConfigConfig

promptRemove :: MonadIO m => Game -> m Bool
promptRemove game = do
    printOutput $ Normal $ "Permanently delete " <> gameName game <> "? (y/N) "
    input <- liftIO getLine
    return $ toLower (head $ if null input then "n" else input) == 'y'

warnMissingGames :: Foldable t => t String -> Sbu ()
warnMissingGames games = do
    cGames <- asks $ configGames . runConfigConfig
    mapM_
        ( \g ->
            when (g `notElem` map gameName cGames) $
                printOutput $ Warning $ "No game named `" <> g <> "'"
        )
        games

getGameByName :: String -> Sbu (Maybe Game)
getGameByName name = do
    games <- asks $ configGames . runConfigConfig
    return $ find ((==) name . gameName) games
