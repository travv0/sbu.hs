module Options
  ( Command(..)
  , BackupOptions(..)
  , AddOptions(..)
  , opts
  )
where

import           Options.Applicative
import           Path

data Command
  = Backup BackupOptions
  | Add AddOptions
  | List
  | Info InfoOptions
  | Remove RemoveOptions
  | Edit EditOptions
  | Config ConfigOptions
  deriving (Show)

data BackupOptions = BackupOptions
  { backupGames :: [String]
  , backupLoop :: Bool
  } deriving (Show)

newtype AddOptions = AddOptions
  { addGames :: [String]
  } deriving (Show)

newtype InfoOptions = InfoOptions
  { infoGames :: [String]
  } deriving (Show)

data RemoveOptions = RemoveOptions
  { removeGames :: [String]
  , removeYes :: Bool
  } deriving (Show)

data EditOptions = EditOptions
  { editGame :: String
  , editName :: String
  , editPath :: Path Abs Dir
  , editGlob :: String
  } deriving (Show)

data ConfigOptions = ConfigOptions
  { configBackupDir :: Path Abs Dir
  , configBackupFreq :: Integer
  , configBackupsToKeep :: Integer
  } deriving (Show)

backupParser :: Parser Command
backupParser =
  Backup
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
addParser = Add . AddOptions <$> some
  (argument str (metavar "GAMES..." <> help "List of games to add"))

infoParser :: Parser Command
infoParser = Info . InfoOptions <$> some
  (argument
    str
    (  metavar "GAMES..."
    <> help
         "List of games to display info for.  If not provided, will display info for all games"
    )
  )

removeParser :: Parser Command
removeParser =
  Remove
    <$> (   RemoveOptions
        <$> many
              (argument
                str
                (metavar "GAMES..." <> help "List of games to remove")
              )
        <*> switch
              (long "yes" <> short 'y' <> help
                "Remove all without confirmation prompts"
              )
        )

opts = info
  (    hsubparser
      (  command
          "backup"
          (info backupParser (fullDesc <> progDesc "Backup your game saves"))
      <> command
           "add"
           (info addParser (fullDesc <> progDesc "Add games to backup"))
      <> command
           "list"
           (info (pure List)
                 (fullDesc <> progDesc "List games that can be backed up")
           )
      )
  <**> helper
  )
  idm
