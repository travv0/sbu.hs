module Types
  ( Config(..)
  )
where

import           Path

data Config = Config
  { configBackupDir :: Path Abs Dir
  , configBackupFreq :: Integer
  , configBackupsToKeep :: Integer
  } deriving (Show)

