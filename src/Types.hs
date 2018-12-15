{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Config(..)
  , Game(..)
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

instance Eq Game where
  a == b = gameName a == gameName b

instance Serialize Config
instance Serialize Game
