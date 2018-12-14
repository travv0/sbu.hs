module Options
  ( Command
  , BackupOptions
  , backupParser
  , opts
  )
where

import           Options.Applicative

data Options = Options
  { optCommand :: Command }
  deriving (Show)

data Command
  = Backup BackupOptions
  | Add AddOptions
  -- | List ListOptions
  -- | Info InfoOptions
  -- | Remove RemoveOptions
  -- | Edit EditOptions
  -- | Config ConfigOptions
  deriving (Show)

data BackupOptions = BackupOptions
  { backupGames :: [String]
  , loop :: Bool
  } deriving (Show)

data AddOptions = AddOptions
  { addGames :: [String]
  } deriving (Show)

backupParser :: Parser Command
backupParser =
  Backup
    <$> (   BackupOptions
        <$> some
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
addParser = Add <$> AddOptions <$> some
  (argument str (metavar "GAMES..." <> help "List of games to add"))

opts = info
  (    hsubparser
      (  command
          "backup"
          (info backupParser (fullDesc <> progDesc "Backup your game saves"))
      <> command
           "add"
           (info addParser (fullDesc <> progDesc "Add games to backup"))
      )
  <**> helper
  )
  idm