{-# LANGUAGE TemplateHaskell #-}

module Options (
    VbuOptions (..),
    Command (..),
    BackupOptions (..),
    AddOptions (..),
    InfoOptions (..),
    RemoveOptions (..),
    EditOptions (..),
    ConfigOptions (..),
    opts,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative (
    ParseError (ShowHelpText),
    Parser,
    ReadM,
    argument,
    command,
    eitherReader,
    flag',
    fullDesc,
    help,
    hsubparser,
    info,
    infoOption,
    long,
    many,
    metavar,
    noArgError,
    option,
    progDesc,
    readerAbort,
    short,
    str,
    strOption,
    switch,
    value,
    (<|>),
 )
import Options.Applicative.Builder.Internal (noGlobal)
import Options.Applicative.NonEmpty (some1)
import Options.Applicative.Types (readerAsk)
import Paths_vbu (version)

data VbuOptions = VbuOptions
    { vbuConfigPath :: Maybe FilePath
    , vbuCommand :: Command
    }
    deriving (Show)

data Command
    = BackupCmd BackupOptions
    | AddCmd AddOptions
    | ListCmd
    | InfoCmd InfoOptions
    | RemoveCmd RemoveOptions
    | EditCmd EditOptions
    | ConfigCmd ConfigOptions
    deriving (Show)

data BackupOptions = BackupOptions
    { backupOptGroups :: [String]
    , backupOptLoop :: Bool
    , backupOptVerbose :: Bool
    }
    deriving (Show)

data AddOptions = AddOptions
    { addOptGroup :: String
    , addOptPath :: FilePath
    , addOptGlob :: Maybe String
    }
    deriving (Show)

newtype InfoOptions = InfoOptions
    { infoOptGroups :: [String]
    }
    deriving (Show)

data RemoveOptions = RemoveOptions
    { removeOptGroups :: NonEmpty String
    , removeOptYes :: Bool
    }
    deriving (Show)

data EditOptions = EditOptions
    { editOptGroup :: String
    , editOptName :: Maybe String
    , editOptPath :: Maybe FilePath
    , editOptGlob :: Maybe String
    }
    deriving (Show)

data ConfigOptions
    = ConfigOptions
        { configOptBackupDir :: Maybe FilePath
        , configOptBackupFreq :: Maybe Integer
        , configOptBackupsToKeep :: Maybe Integer
        }
    | ConfigDefaults
    deriving (Show)

backupParser :: Parser Command
backupParser =
    BackupCmd
        <$> ( BackupOptions
                <$> many
                    ( argument
                        str
                        ( mconcat
                            [ metavar "GROUPS..."
                            , help
                                "List of groups to back up.  If not provided, will back up all groups"
                            ]
                        )
                    )
                <*> switch
                    ( mconcat
                        [ long "loop"
                        , short 'l'
                        , help
                            "Keep running, backing up groups at the interval specified in your config file"
                        ]
                    )
                <*> switch
                    ( mconcat
                        [ long "verbose"
                        , short 'v'
                        , help "Print verbose output"
                        ]
                    )
            )

addParser :: Parser Command
addParser =
    AddCmd
        <$> ( AddOptions
                <$> argument str (metavar "GROUP")
                <*> strOption
                    ( mconcat
                        [ long "path"
                        , short 'p'
                        , metavar "PATH"
                        , help "Path to added group's files"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "glob"
                        , short 'g'
                        , metavar "FILE_GLOB"
                        , value Nothing
                        , help
                            "File glob for added group's files. \
                            \Only files matching this pattern will be backed up. \
                            \The default is **/* which will recursively back up \
                            \all files in PATH"
                        ]
                    )
            )

infoParser :: Parser Command
infoParser =
    InfoCmd . InfoOptions
        <$> many
            ( argument
                str
                ( mconcat
                    [ metavar "GROUPS..."
                    , help
                        "List of groups to display info for.  If not provided, will display \
                        \info for all groups"
                    ]
                )
            )

removeParser :: Parser Command
removeParser =
    RemoveCmd
        <$> ( RemoveOptions
                <$> some1
                    ( argument
                        str
                        ( mconcat
                            [ metavar "GROUPS..."
                            , help "List of groups to remove"
                            ]
                        )
                    )
                <*> switch
                    ( mconcat
                        [ long "yes"
                        , short 'y'
                        , help
                            "Remove all without confirmation prompts"
                        ]
                    )
            )

editParser :: Parser Command
editParser =
    EditCmd
        <$> ( EditOptions
                <$> argument str (metavar "GROUP")
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "name"
                        , short 'n'
                        , metavar "NEW_NAME"
                        , value Nothing
                        , help
                            "Set group name to NEW_NAME. This will also update the directory \
                            \name in your backup directory"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "path"
                        , short 'p'
                        , metavar "NEW_PATH"
                        , value Nothing
                        , help "Set group's path to NEW_PATH"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "glob"
                        , short 'g'
                        , metavar "NEW_FILE_GLOB"
                        , value Nothing
                        , help "Set group's file glob to NEW_FILE_GLOB. Setting this to an empty string or \"none\" implies the glob **/* which will recursively back up all files"
                        ]
                    )
            )

configParser :: Parser Command
configParser =
    ConfigCmd
        <$> ( ConfigOptions
                <$> option
                    maybeStr
                    ( mconcat
                        [ long "path"
                        , short 'p'
                        , metavar "BACKUP_PATH"
                        , value Nothing
                        , help "Set path to directory in which to back up files"
                        ]
                    )
                <*> option
                    maybeAuto
                    ( mconcat
                        [ long "frequency"
                        , short 'f'
                        , metavar "BACKUP_FREQUENCY"
                        , value Nothing
                        , help "Set frequency in minutes to backup files when looping"
                        ]
                    )
                <*> option
                    maybeAuto
                    ( mconcat
                        [ long "keep"
                        , short 'k'
                        , metavar "BACKUPS_TO_KEEP"
                        , value Nothing
                        , help "Set how many copies of each backed-up file to keep"
                        ]
                    )
                <|> flag'
                    ConfigDefaults
                    ( mconcat
                        [ long "use-defaults"
                        , help "Reset config to use default values"
                        ]
                    )
            )

opts :: Parser VbuOptions
opts =
    VbuOptions
        <$> option
            maybeStr
            ( mconcat
                [ long "config"
                , short 'c'
                , metavar "CONFIG_FILE"
                , value Nothing
                , help "Path to configuration file"
                ]
            )
        <*> ( infoOption
                (concat ["vbu v", showVersion version, " (rev: ", take 7 $(gitHash), ")"])
                ( mconcat
                    [ long "version"
                    , help "Print version information"
                    ]
                )
                <*> helper
                <*> commands
            )

commands :: Parser Command
commands =
    hsubparser $
        mconcat
            [ command
                "backup"
                (info backupParser (fullDesc <> progDesc "Backup your files"))
            , command
                "add"
                (info addParser (fullDesc <> progDesc "Add groups to backup"))
            , command
                "list"
                ( info
                    (pure ListCmd)
                    (fullDesc <> progDesc "List groups that can be backed up")
                )
            , command
                "info"
                (info infoParser (fullDesc <> progDesc "List info for groups"))
            , command
                "remove"
                ( info
                    removeParser
                    (fullDesc <> progDesc "Remove groups from backup")
                )
            , command
                "edit"
                (info editParser (fullDesc <> progDesc "Edit group info"))
            , command
                "config"
                ( info
                    configParser
                    (fullDesc <> progDesc "Manage vbu configuration")
                )
            ]

maybeStr :: IsString a => ReadM (Maybe a)
maybeStr = Just <$> str

maybeAuto :: Read a => ReadM (Maybe a)
maybeAuto = eitherReader $ \arg -> case reads arg of
    [(r, "")] -> return $ Just r
    _ -> Left $ "cannot parse value `" ++ arg ++ "'"

helper :: Parser (a -> a)
helper =
    option helpReader $
        mconcat
            [ long "help"
            , short 'h'
            , help "Show this help text"
            , value id
            , metavar ""
            , noGlobal
            , noArgError (ShowHelpText Nothing)
            ]
  where
    helpReader = do
        potentialCommand <- readerAsk
        readerAbort $
            ShowHelpText (Just potentialCommand)
