{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Types (
    Config (..),
    RunConfig (..),
    Game (..),
    Sbu,
    Output (..),
    runSbu,
) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

type Sbu = ReaderT RunConfig IO

runSbu :: Sbu a -> RunConfig -> IO a
runSbu = runReaderT

data Output = Normal String | Info String | Warning String | Error String
    deriving (Show, Eq)

data RunConfig = RunConfig
    { runConfigConfig :: Config
    , runConfigVerbose :: Bool
    }

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
