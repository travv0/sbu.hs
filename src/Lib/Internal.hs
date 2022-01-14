{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib.Internal
    ( defaultConfigPath
    , readConfig
    , createDefaultConfig
    , handleCommand
    , maybeWriteConfig
    ) where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( filterM
                                                , foldM
                                                , unless
                                                , when
                                                )
import           Control.Monad.Catch            ( MonadMask
                                                , catchIOError
                                                , finally
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.ListM            ( sortByM )
import           Control.Monad.Reader           ( ask
                                                , asks
                                                , local
                                                )
import qualified Data.ByteString               as BS
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( find
                                                , toList
                                                , traverse_
                                                )
import           Data.List                      ( elemIndex
                                                , intercalate
                                                , isPrefixOf
                                                , sort
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Serialize                 ( decode
                                                , encode
                                                )
import           Data.Set                       ( Set )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , diffUTCTime
                                                , formatTime
                                                , getCurrentTime
                                                , getCurrentTimeZone
                                                , secondsToDiffTime
                                                , utcToLocalTime
                                                , utctDayTime
                                                )
import           GHC.Exts                       ( fromList )
import           Options                        ( AddOptions(..)
                                                , BackupOptions(..)
                                                , Command(..)
                                                , ConfigOptions(..)
                                                , EditOptions(..)
                                                , InfoOptions(..)
                                                , RemoveOptions(..)
                                                )
import           Prettyprinter                  ( Doc
                                                , Pretty
                                                , annotate
                                                , pretty
                                                )
import           Prettyprinter.Render.Terminal  ( AnsiStyle
                                                , Color(..)
                                                , color
                                                , hPutDoc
                                                )
import           System.Directory               ( canonicalizePath
                                                , copyFileWithMetadata
                                                , createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getAppUserDataDirectory
                                                , getHomeDirectory
                                                , getModificationTime
                                                , listDirectory
                                                , pathIsSymbolicLink
                                                , removeFile
                                                , renameDirectory
                                                , renameFile
                                                )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                , addTrailingPathSeparator
                                                , dropFileName
                                                , isRelative
                                                , takeDirectory
                                                )
import           System.FilePath.Glob           ( compile
                                                , globDir1
                                                , matchDefault
                                                , matchDotsImplicitly
                                                , matchWith
                                                )
import           System.IO                      ( Handle
                                                , hFlush
                                                , hPutStrLn
                                                , stderr
                                                , stdout
                                                )
import           Types                          ( Config(..)
                                                , Group(..)
                                                , Output(..)
                                                , RunConfig(..)
                                                , Vbu
                                                )

defaultGlob :: String
defaultGlob = "**/*"

defaultConfigDir :: IO FilePath
defaultConfigDir = getAppUserDataDirectory "vbu"

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
    locked   <- doesFileExist lockPath
    if locked
        then do
            hPutStrLn stderr
                $ "vbu appears to be already running.  If it's not, delete the file `"
                <> lockPath
                <> "' and try again"
            liftIO exitFailure
        else do
            createDirectoryIfMissing True $ dropFileName lockPath
            BS.writeFile lockPath BS.empty

deleteLockFile :: IO ()
deleteLockFile = do
    lockPath <- lockFilePath
    locked   <- doesFileExist lockPath
    when locked $ removeFile lockPath

withLockFile :: (MonadIO m, MonadMask m) => m () -> m ()
withLockFile f = do
    liftIO createLockFile
    f `finally` liftIO deleteLockFile

defaultConfig :: IO Config
defaultConfig = do
    home <- getHomeDirectory
    return $ Config (home </> "vbu_backups") 15 20 []

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
    hPutStrLn stderr
        $  "Creating new config file at `"
        <> path
        <> "'.\n"
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
outputToDoc (Normal  s) = (stdout, pretty s)
outputToDoc (Info    s) = (stderr, infoText $ T.pack s)
outputToDoc (Warning s) = (stderr, warningText $ T.pack s)
outputToDoc (Error   s) = (stderr, errorText $ T.pack s)

info :: MonadIO m => Bool -> String -> m ()
info verbose = when verbose . printOutput . Info

warn :: MonadIO m => String -> m ()
warn = printOutput . Warning

err :: MonadIO m => String -> m ()
err = printOutput . Error

prn :: MonadIO m => String -> m ()
prn = printOutput . Normal

handleCommand :: Command -> Vbu (Maybe Config)
handleCommand (AddCmd (AddOptions group path glob)) = do
    path' <- liftIO $ canonicalizePath' path
    addGroup group path' glob
handleCommand ListCmd = listGroups
handleCommand (InfoCmd (InfoOptions groups)) = infoGroups groups
handleCommand (RemoveCmd (RemoveOptions groups yes)) = removeGroups yes groups
handleCommand (EditCmd (EditOptions group mNewName mNewPath mNewGlob)) = do
    mNewPath' <- liftIO $ traverse canonicalizePath' mNewPath
    editGroup group mNewName mNewPath' mNewGlob
handleCommand (ConfigCmd (ConfigOptions mBackupDir mBackupFreq mBackupsToKeep))
    = do
        config       <- asks runConfigConfig
        newBackupDir <- liftIO $ canonicalizePath' $ fromMaybe
            (configBackupDir config)
            mBackupDir
        editConfig newBackupDir mBackupFreq mBackupsToKeep
handleCommand (ConfigCmd ConfigDefaults) = do
    dc <- liftIO defaultConfig
    editConfig (configBackupDir dc)
               (Just $ configBackupFreq dc)
               (Just $ configBackupsToKeep dc)
handleCommand (BackupCmd (BackupOptions groups loop verbose)) =
    local (\c -> c { runConfigVerbose = verbose }) $ backupGroups loop groups

validGroupNameChars :: Set Char
validGroupNameChars =
    fromList $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['-', '_']

isValidGroupName :: String -> Bool
isValidGroupName = all (`elem` validGroupNameChars)

addGroup :: String -> FilePath -> Maybe String -> Vbu (Maybe Config)
addGroup group path glob = do
    config <- asks runConfigConfig
    if
        | group `elem` map groupName (configGroups config) -> do
            err $ "Group with the name " <> group <> " already exists"
            return Nothing
        | not $ isValidGroupName group -> do
            err
                $ "Invalid characters in name `"
                <> group
                <> "': only alphanumeric characters, underscores, and hyphens are allowed"
            return Nothing
        | otherwise -> if isRelative path
            then do
                err
                    $  "Path must be absolute, but relative path was supplied: "
                    <> path
                return Nothing
            else do
                pathExists <- liftIO $ doesDirectoryExist path
                unless pathExists $ warn $ "Path doesn't exist: " <> path
                let newGlob = case fromMaybe "" glob of
                        "none" -> ""
                        g      -> g
                    newGroup = Group group path newGlob
                prn "Group added successfully:\n"
                liftIO $ printGroup newGroup Nothing Nothing Nothing
                return $ Just $ config
                    { configGroups = newGroup : configGroups config
                    }

canonicalizePath' :: FilePath -> IO FilePath
canonicalizePath' ('~' : path) = do
    let trimmedPath = dropWhile (\c -> c == '/' || c == '\\') path
    homeDir <- getHomeDirectory
    canonicalizePath $ homeDir </> trimmedPath
canonicalizePath' path = canonicalizePath path

listGroups :: Vbu (Maybe Config)
listGroups = do
    gNames <- groupNames
    prn $ intercalate "\n" gNames
    return Nothing

removeGroups :: Bool -> NonEmpty String -> Vbu (Maybe Config)
removeGroups yes groups = do
    config <- asks runConfigConfig
    if yes
        then do
            prn $ "Removed the following groups:\n" <> intercalate
                "\n"
                (toList groups)
            return $ Just $ config
                { configGroups = filter ((`notElem` groups) . groupName)
                                     $ configGroups config
                }
        else do
            warnMissingGroups groups
            groupsToRemove <-
                filterM promptRemove
                $ filter ((`elem` groups) . groupName)
                $ configGroups config
            mapM_ (\g -> prn $ "Removed " <> groupName g) groupsToRemove
            return $ Just $ config
                { configGroups =
                    filter
                            ( (`notElem` map groupName groupsToRemove)
                            . groupName
                            )
                        $ configGroups config
                }

infoGroups :: [String] -> Vbu (Maybe Config)
infoGroups groups = do
    allGroupNames <- groupNames
    mapM_ infoGroup $ if null groups then allGroupNames else groups
    return Nothing

editGroup
    :: String
    -> Maybe String
    -> Maybe FilePath
    -> Maybe String
    -> Vbu (Maybe Config)
editGroup _ Nothing Nothing Nothing = do
    err "One or more of --name, --path, or --glob must be provided."
    return Nothing
editGroup gName mNewName mNewPath mNewGlob = do
    config <- asks runConfigConfig
    let i          = elemIndex gName $ map groupName (configGroups config)
        mSplitList = splitAt <$> i <*> pure (configGroups config)
    case mSplitList of
        Nothing -> do
            warnMissingGroups [gName]
            return Nothing
        Just (_    , []          ) -> error "Couldn't find group in list"
        Just (front, group : back) -> do
            liftIO $ traverse_
                (\path -> do
                    pathExists <- doesDirectoryExist path
                    unless pathExists $ warn $ "Path doesn't exist: " <> path
                )
                mNewPath
            let newName = fromMaybe (groupName group) mNewName
                newPath = fromMaybe (groupPath group) mNewPath
                newGlob = fmap
                    (\case
                        "none" -> ""
                        glob   -> glob
                    )
                    mNewGlob
            let editedGroup = group
                    { groupName = newName
                    , groupPath = newPath
                    , groupGlob = fromMaybe (groupGlob group) newGlob
                    }
            if
                | isRelative newPath
                -> do
                    err
                        $ "Path must be absolute, but relative path was supplied: "
                        <> newPath
                    return Nothing
                | not $ isValidGroupName newName
                -> do
                    err
                        $ "Invalid characters in name `"
                        <> newName
                        <> "': only alphanumeric characters, `_', `-', and `/' are allowed"
                    return Nothing
                | otherwise
                -> do
                    liftIO $ printGroup group
                                        (Just newName)
                                        (Just newPath)
                                        newGlob
                    backupDirExists <-
                        liftIO
                        $   doesDirectoryExist
                        $   configBackupDir config
                        </> gName
                    when (isJust mNewName && backupDirExists) $ do
                        warn "Group name changed, renaming backup directory..."
                        liftIO $ renameDirectory
                            (configBackupDir config </> gName)
                            (configBackupDir config </> newName)

                    return $ Just $ config
                        { configGroups = front <> (editedGroup : back)
                        }

printConfigRow :: MonadIO m => String -> String -> Maybe String -> m ()
printConfigRow label val newVal = prn $ label <> ": " <> val <> case newVal of
    Just nv | val == nv -> ""
            | otherwise -> " -> " <> nv
    Nothing -> ""

printConfig
    :: MonadIO m
    => Config
    -> Maybe String
    -> Maybe Integer
    -> Maybe Integer
    -> m ()
printConfig config mNewBackupDir mNewBackupFreq mNewBackupsToKeep = do
    printConfigRow "Backup path" (configBackupDir config) mNewBackupDir
    printConfigRow "Backup frequency (in minutes)"
                   (show $ configBackupFreq config)
                   (show <$> mNewBackupFreq)
    printConfigRow "Number of backups to keep"
                   (show $ configBackupsToKeep config)
                   (show <$> mNewBackupsToKeep)
    prn ""

editConfig :: FilePath -> Maybe Integer -> Maybe Integer -> Vbu (Maybe Config)
editConfig newBackupDir mBackupFreq mBackupsToKeep = do
    config <- asks runConfigConfig
    let newBackupFreq = fromMaybe (configBackupFreq config) mBackupFreq
        newBackupsToKeep =
            fromMaybe (configBackupsToKeep config) mBackupsToKeep

    if isRelative newBackupDir
        then do
            err
                $ "Backup path must be absolute, but relative path was supplied: "
                <> newBackupDir
            return Nothing
        else do
            liftIO $ printConfig config
                                 (Just newBackupDir)
                                 mBackupFreq
                                 mBackupsToKeep
            return $ Just $ config { configBackupDir     = newBackupDir
                                   , configBackupFreq    = newBackupFreq
                                   , configBackupsToKeep = newBackupsToKeep
                                   }

backupGroups :: Bool -> [String] -> Vbu (Maybe Config)
backupGroups loop groups = do
    RunConfig { runConfigConfig = config, runConfigVerbose = verbose } <- ask
    allGroupNames <- groupNames
    let groupsToBackup = if null groups then allGroupNames else groups
    warnings <- foldM
        (\acc group -> do
            warnings <- backupGroup group `catchIOError` \e -> do
                err $ "Error backing up " <> group <> ": " <> show e
                return []
            return $ acc <> warnings
        )
        []
        groupsToBackup
    unless (null warnings) $ do
        let msg =
                show (length warnings)
                    <> " warning"
                    <> (if length warnings == 1 then "" else "s")
                    <> " occurred:"
        if verbose
            then do
                warn $ msg <> "\n"
                mapM_ warn warnings
            else
                warn
                $ msg
                <> "\nPass --verbose flag to print all warnings after backup completes\n"
    if loop
        then do
            liftIO
                $ threadDelay
                $ fromIntegral
                $ configBackupFreq config
                * 60
                * 1000000
            backupGroups loop groups
        else return Nothing

backupGroup :: String -> Vbu [String]
backupGroup gName = do
    config    <- asks runConfigConfig
    startTime <- liftIO getCurrentTime
    mGroup    <- getGroupByName gName
    case mGroup of
        Just group -> do
            isDirectory <- liftIO $ doesDirectoryExist $ groupPath group
            if isDirectory
                then do
                    (backedUpCount, warnings) <- backupFiles
                        (groupName group)
                        (groupPath group)
                        (groupGlob group)
                        (groupPath group)
                        (configBackupDir config </> gName)
                    now <- liftIO getCurrentTime
                    tz  <- liftIO getCurrentTimeZone
                    when (backedUpCount > 0)
                        $  prn
                        $  "\nFinished backing up "
                        <> show backedUpCount
                        <> " file"
                        <> (if backedUpCount == 1 then "" else "s")
                        <> (if null warnings
                               then ""
                               else
                                   " with "
                                   <> show (length warnings)
                                   <> " warning"
                                   <> (if length warnings == 1 then "" else "s")
                           )
                        <> " for "
                        <> gName
                        <> " in "
                        <> show (diffUTCTime now startTime)
                        <> " on "
                        <> formatTime defaultTimeLocale
                                      "%c"
                                      (utcToLocalTime tz now)
                        <> "\n"
                    return warnings
                else do
                    warn
                        $  "Path set for "
                        <> gName
                        <> " doesn't exist: "
                        <> groupPath group
                    return []
        Nothing -> do
            warnMissingGroups [gName]
            return []

backupFiles
    :: String
    -> FilePath
    -> String
    -> FilePath
    -> FilePath
    -> Vbu (Integer, [String])
backupFiles group basePath glob from to = do
    files <- liftIO $ listDirectory from
    foldM
        (\(c, es) f -> do
            (newCount, newErrs) <- backupFile group
                                              basePath
                                              glob
                                              (from </> f)
                                              (to </> f)
            return (c + newCount, es <> newErrs)
        )
        (0, [])
        files

backupFile
    :: String
    -> FilePath
    -> String
    -> FilePath
    -> FilePath
    -> Vbu (Integer, [String])
backupFile group basePath glob from to = do
    isDirectory <- liftIO $ doesDirectoryExist from
    if isDirectory
        then backupFiles group basePath glob from to
        else if globMatches then backupFile' else return (0, [])
  where
    globMatches = matchWith
        (matchDefault { matchDotsImplicitly = True })
        (compile $ addTrailingPathSeparator basePath <> if null glob
            then defaultGlob
            else glob
        )
        from
    backupFile' =
        do
            verbose   <- asks runConfigVerbose
            isSymlink <- liftIO $ pathIsSymbolicLink from
            if isSymlink
                then do
                    info verbose
                        $ from
                        <> " appears to be a link to somewhere else in the filesystem. Skipping..."
                    return (0, [])
                else do
                    backupExists <- liftIO $ doesFileExist to
                    fromModTime  <- liftIO $ getModificationTime from
                    mToModTime   <- if backupExists
                        then liftIO $ Just <$> getModificationTime to
                        else return Nothing
                    case mToModTime of
                        Just toModTime ->
                            if fromModTime
                                        { utctDayTime =
                                            secondsToDiffTime
                                            $ round
                                            $ utctDayTime fromModTime
                                        }
                                    /= toModTime
                                           { utctDayTime =
                                               secondsToDiffTime
                                               $ round
                                               $ utctDayTime toModTime
                                           }
                                then do
                                    liftIO
                                        $   renameFile to
                                        $   to
                                        <.> "bak"
                                        <.> formatModifiedTime toModTime
                                    copyAndCleanup
                                else return (0, [])
                        Nothing -> copyAndCleanup
        `catchIOError` \e -> do
                           let warning =
                                   "Unable to backup file "
                                       <> to
                                       <> " for group "
                                       <> group
                                       <> ":\n"
                                       <> show e
                                       <> "\n"
                           warn warning
                           return (1, [warning])
    copyAndCleanup = do
        liftIO $ createDirectoryIfMissing True $ dropFileName to
        prn $ from <> " ==>\n    " <> to
        liftIO $ copyFileWithMetadata from to
        cleanupBackups to
        return (1, [])

cleanupBackups :: FilePath -> Vbu ()
cleanupBackups backupPath = do
    RunConfig { runConfigConfig = config, runConfigVerbose = verbose } <- ask
    when (configBackupsToKeep config > 0) $ do
        files <-
            liftIO
            $   (backupPath :)
            .   filter (backupPath `isPrefixOf`)
            <$> globDir1
                    (compile
                        "*.bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
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
            let filesToDelete = drop
                    (fromIntegral $ configBackupsToKeep config)
                    sortedFiles
            mapM_
                (\f -> do
                    info verbose $ "Deleting " <> f
                    liftIO $ removeFile f
                )
                filesToDelete

formatModifiedTime :: UTCTime -> String
formatModifiedTime = formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S"

infoGroup :: String -> Vbu ()
infoGroup gName = do
    cGroups <- asks $ configGroups . runConfigConfig
    let matchingGroups = filter (\g -> groupName g == gName) cGroups
    case matchingGroups of
        []        -> warnMissingGroups [gName]
        group : _ -> printGroup group Nothing Nothing Nothing

printGroup
    :: MonadIO m
    => Group
    -> Maybe String
    -> Maybe FilePath
    -> Maybe String
    -> m ()
printGroup group mNewName mNewPath mNewGlob = do
    printConfigRow "Name" (groupName group) mNewName
    printConfigRow "Path" (groupPath group) mNewPath
    when (not (null (groupGlob group)) || isJust mNewGlob)
        $ printConfigRow "Glob" (groupGlob group) mNewGlob
    prn ""

groupNames :: Vbu [String]
groupNames = asks $ sort . map groupName . configGroups . runConfigConfig

promptRemove :: MonadIO m => Group -> m Bool
promptRemove group = do
    prn $ "Permanently delete " <> groupName group <> "? (y/N) "
    input <- liftIO getLine
    return $ toLower (head $ if null input then "n" else input) == 'y'

warnMissingGroups :: Foldable t => t String -> Vbu ()
warnMissingGroups groups = do
    cGroups <- asks $ configGroups . runConfigConfig
    mapM_
        (\g ->
            when (g `notElem` map groupName cGroups)
                $  warn
                $  "No group named `"
                <> g
                <> "'"
        )
        groups

getGroupByName :: String -> Vbu (Maybe Group)
getGroupByName name = do
    groups <- asks $ configGroups . runConfigConfig
    return $ find ((==) name . groupName) groups
