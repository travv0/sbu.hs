module Options
  ( SbuOptions(..)
  , Command(..)
  , BackupOptions(..)
  , AddOptions(..)
  , InfoOptions(..)
  , RemoveOptions(..)
  , EditOptions(..)
  , ConfigOptions(..)
  , opts
  )
where

import           Data.String                    ( IsString )

import           Options.Applicative     hiding ( infoParser )

version :: String
version = "1.2.0"

data SbuOptions = SbuOptions
  { sbuConfigPath :: Maybe FilePath
  , sbuCommand :: Command
  } deriving (Show)

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
  } deriving (Show)

newtype AddOptions = AddOptions
  { addOptGames :: [String]
  } deriving (Show)

newtype InfoOptions = InfoOptions
  { infoOptGames :: [String]
  } deriving (Show)

data RemoveOptions = RemoveOptions
  { removeOptGames :: [String]
  , removeOptYes :: Bool
  } deriving (Show)

data EditOptions = EditOptions
  { editOptGame :: String
  , editOptName :: Maybe String
  , editOptPath :: Maybe FilePath
  , editOptGlob :: Maybe String
  } deriving (Show)

data ConfigOptions = ConfigOptions
  { configOptBackupDir :: Maybe FilePath
  , configOptBackupFreq :: Maybe Integer
  , configOptBackupsToKeep :: Maybe Integer
  }
    | ConfigDefaults
    deriving (Show)

backupParser :: Parser Command
backupParser =
  BackupCmd
    <$> (   BackupOptions
        <$> many
              (argument
                str
                (  metavar "GAMES..."
                <> help
                     "List of games to back up.  If not provided, will back up all games"
                )
              )
        <*> switch
              (  long "loop"
              <> short 'l'
              <> help
                   "Keep running, backing up games at the interval specified in your config file"
              )
        )

addParser :: Parser Command
addParser = AddCmd . AddOptions <$> some
  (argument str (metavar "GAMES..." <> help "List of games to add"))

infoParser :: Parser Command
infoParser = InfoCmd . InfoOptions <$> many
  (argument
    str
    (  metavar "GAMES..."
    <> help
         "List of games to display info for.  If not provided, will display info for all games"
    )
  )

removeParser :: Parser Command
removeParser =
  RemoveCmd
    <$> (   RemoveOptions
        <$> some
              (argument
                str
                (metavar "GAMES..." <> help "List of games to remove")
              )
        <*> switch
              (long "yes" <> short 'y' <> help
                "Remove all without confirmation prompts"
              )
        )

editParser :: Parser Command
editParser =
  EditCmd
    <$> (   EditOptions
        <$> argument str (metavar "GAME")
        <*> option
              maybeStr
              (  long "name"
              <> short 'n'
              <> metavar "NEW_NAME"
              <> value Nothing
              <> help "New name"
              )
        <*> option
              maybeStr
              (  long "path"
              <> short 'p'
              <> metavar "NEW_SAVE_PATH"
              <> value Nothing
              <> help "New save path"
              )
        <*> option
              maybeStr
              (  long "glob"
              <> short 'g'
              <> metavar "NEW_SAVE_FILE_GLOB"
              <> value Nothing
              <> help "New save file glob"
              )
        )

configParser :: Parser Command
configParser =
  ConfigCmd
    <$> (   ConfigOptions
        <$> option
              maybeStr
              (  long "path"
              <> short 'p'
              <> metavar "BACKUP_PATH"
              <> value Nothing
              <> help "Path to directory in which to back up saves"
              )
        <*> option
              maybeAuto
              (  long "frequency"
              <> short 'f'
              <> metavar "BACKUP_FREQUENCY"
              <> value Nothing
              <> help "Frequency in minutes to backup saves when looping"
              )
        <*> option
              maybeAuto
              (  long "keep"
              <> short 'k'
              <> metavar "BACKUPS_TO_KEEP"
              <> value Nothing
              <> help "How many copies of each backed-up file to keep"
              )
        <|> flag'
              ConfigDefaults
              (  long "use-defaults"
              <> short 'd'
              <> help
                   "Keep running, backing up games at the interval specified in your config file"
              )
        )

opts :: Parser SbuOptions
opts =
  SbuOptions
    <$> option
          maybeStr
          (  long "config"
          <> short 'c'
          <> metavar "CONFIG_FILE"
          <> value Nothing
          <> help "Path to configuration file"
          )
    <*> (   infoOption
            ("sbu version " ++ version)
            (long "version" <> short 'v' <> help "Print version information")
        <*> commands
        )

commands :: Parser Command
commands =
  hsubparser
      (  command
          "backup"
          (info backupParser (fullDesc <> progDesc "Backup your game saves"))
      <> command
           "add"
           (info addParser (fullDesc <> progDesc "Add games to backup"))
      <> command
           "list"
           (info (pure ListCmd)
                 (fullDesc <> progDesc "List games that can be backed up")
           )
      <> command
           "info"
           (info infoParser (fullDesc <> progDesc "List info for games"))
      <> command
           "remove"
           (info removeParser
                 (fullDesc <> progDesc "Remove games from backup")
           )
      <> command "edit"
                 (info editParser (fullDesc <> progDesc "Edit game info"))
      <> command
           "config"
           (info configParser
                 (fullDesc <> progDesc "Manage sbu configuration")
           )
      )
    <**> helper

maybeStr :: IsString a => ReadM (Maybe a)
maybeStr = Just <$> str

maybeAuto :: Read a => ReadM (Maybe a)
maybeAuto = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return $ Just r
  _         -> Left $ "cannot parse value `" ++ arg ++ "'"
