{-# LANGUAGE DeriveGeneric #-}

module Types (
    Config (..),
    Game (..),
    Sbu,
    Logger,
) where

import Control.Monad.Reader (ReaderT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Pipes (Producer)

type Sbu = Logger (ReaderT Config IO) ()

type Logger m = Producer String m

data Config = Config
    { configBackupDir :: FilePath
    , configBackupFreq :: Integer
    , configBackupsToKeep :: Integer
    , configGames :: [Game]
    }
    deriving (Show, Generic)

data Game = Game
    { gameName :: String
    , gamePath :: FilePath
    , gameGlob :: String
    }
    deriving (Show, Generic)

instance Eq Game where
    a == b = gameName a == gameName b

instance Serialize Config
instance Serialize Game
