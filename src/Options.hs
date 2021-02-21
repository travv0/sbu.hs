{-# LANGUAGE TemplateHaskell #-}

module Options (
    SbuOptions (..),
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
import Paths_sbu (version)

data SbuOptions = SbuOptions
    { sbuConfigPath :: Maybe FilePath
    , sbuCommand :: Command
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
    { backupOptGames :: [String]
    , backupOptLoop :: Bool
    , backupOptVerbose :: Bool
    , backupOptLog :: Bool
    }
    deriving (Show)

data AddOptions = AddOptions
    { addOptGame :: String
    , addOptPath :: FilePath
    , addOptGlob :: Maybe String
    }
    deriving (Show)

newtype InfoOptions = InfoOptions
    { infoOptGames :: [String]
    }
    deriving (Show)

data RemoveOptions = RemoveOptions
    { removeOptGames :: NonEmpty String
    , removeOptYes :: Bool
    }
    deriving (Show)

data EditOptions = EditOptions
    { editOptGame :: String
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
                            [ metavar "GAMES..."
                            , help
                                "List of games to back up.  If not provided, will back up all games"
                            ]
                        )
                    )
                <*> switch
                    ( mconcat
                        [ long "loop"
                        , short 'l'
                        , help
                            "Keep running, backing up games at the interval specified in your config file"
                        ]
                    )
                <*> switch
                    ( mconcat
                        [ long "verbose"
                        , short 'v'
                        , help "Print verbose output"
                        ]
                    )
                <*> switch
                    ( mconcat
                        [ long "log"
                        , help "Log output to file in config directory"
                        ]
                    )
            )

addParser :: Parser Command
addParser =
    AddCmd
        <$> ( AddOptions
                <$> argument str (metavar "GAME")
                <*> strOption
                    ( mconcat
                        [ long "path"
                        , short 'p'
                        , metavar "SAVE_PATH"
                        , help "Path to added game's save files"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "glob"
                        , short 'g'
                        , metavar "SAVE_FILE_GLOB"
                        , value Nothing
                        , help
                            "Save file glob for added game's save files. \
                            \Only files matching this pattern will be backed up. \
                            \The default is **/* which will recursively back up \
                            \all files in SAVE_PATH"
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
                    [ metavar "GAMES..."
                    , help
                        "List of games to display info for.  If not provided, will display \
                        \info for all games"
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
                            [ metavar "GAMES..."
                            , help "List of games to remove"
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
                <$> argument str (metavar "GAME")
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "name"
                        , short 'n'
                        , metavar "NEW_NAME"
                        , value Nothing
                        , help
                            "Set game name to NEW_NAME. This will also update the directory \
                            \name in your backup directory"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "path"
                        , short 'p'
                        , metavar "NEW_SAVE_PATH"
                        , value Nothing
                        , help "Set game's save path to NEW_SAVE_PATH"
                        ]
                    )
                <*> option
                    maybeStr
                    ( mconcat
                        [ long "glob"
                        , short 'g'
                        , metavar "NEW_SAVE_FILE_GLOB"
                        , value Nothing
                        , help "Set game's save file glob to NEW_SAVE_FILE_GLOB. Setting this to an empty string or \"none\" implies the glob **/* which will recursively back up all files"
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
                        , help "Set path to directory in which to back up saves"
                        ]
                    )
                <*> option
                    maybeAuto
                    ( mconcat
                        [ long "frequency"
                        , short 'f'
                        , metavar "BACKUP_FREQUENCY"
                        , value Nothing
                        , help "Set frequency in minutes to backup saves when looping"
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

opts :: Parser SbuOptions
opts =
    SbuOptions
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
                (concat ["sbu v", showVersion version, " (rev: ", take 7 $(gitHash), ")"])
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
                (info backupParser (fullDesc <> progDesc "Backup your game saves"))
            , command
                "add"
                (info addParser (fullDesc <> progDesc "Add games to backup"))
            , command
                "list"
                ( info
                    (pure ListCmd)
                    (fullDesc <> progDesc "List games that can be backed up")
                )
            , command
                "info"
                (info infoParser (fullDesc <> progDesc "List info for games"))
            , command
                "remove"
                ( info
                    removeParser
                    (fullDesc <> progDesc "Remove games from backup")
                )
            , command
                "edit"
                (info editParser (fullDesc <> progDesc "Edit game info"))
            , command
                "config"
                ( info
                    configParser
                    (fullDesc <> progDesc "Manage sbu configuration")
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
