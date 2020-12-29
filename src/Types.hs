{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Types (
    Config (..),
    Game (..),
    Sbu,
    Logger,
) where

import Control.Monad.Reader (ReaderT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Pipes (Producer')

type Sbu = ReaderT Config IO (Maybe Config)

type Logger m a = Producer' String m a

data Config = Config
    { configBackupDir :: FilePath
    , configBackupFreq :: Integer
    , configBackupsToKeep :: Integer
    , configGames :: [Game]
    }
    deriving (Show, Generic, Eq)

data Game = Game
    { gameName :: String
    , gamePath :: FilePath
    , gameGlob :: String
    }
    deriving (Show, Generic, Eq)

instance Serialize Config
instance Serialize Game
