{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Types
  ( Config(..)
  )
where

import           Data.Serialize
import           GHC.Generics

data Config = Config
  { configBackupDir :: FilePath
  , configBackupFreq :: Integer
  , configBackupsToKeep :: Integer
  , configGames :: [Game]
  } deriving (Show, Generic)

data Game = Game
  { gameName :: String
  , gamePath :: FilePath
  , gameGlob :: String
  } deriving (Show, Generic)

instance Serialize Config
instance Serialize Game
