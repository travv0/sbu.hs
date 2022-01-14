{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Types
    ( Config(..)
    , Group(..)
    , Vbu
    , Output(..)
    , runVbu
    ) where

import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Data.Serialize                 ( Serialize )
import           GHC.Generics                   ( Generic )

type Vbu = ReaderT Config IO

runVbu :: Vbu a -> Config -> IO a
runVbu = runReaderT

data Output = Normal String | Info String | Warning String | Error String
    deriving (Show, Eq)

data Config = Config
    { configBackupDir     :: FilePath
    , configBackupFreq    :: Integer
    , configBackupsToKeep :: Integer
    , configGroups        :: [Group]
    }
    deriving (Show, Generic, Eq)

data Group = Group
    { groupName :: String
    , groupPath :: FilePath
    , groupGlob :: String
    }
    deriving (Show, Generic, Eq)

instance Serialize Config
instance Serialize Group
