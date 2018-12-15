{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( handleOptions
  )
where

import           Data.Maybe
import           Path
import           System.Directory

import           Options
import           Types

defaultConfigPath :: IO (Path Abs File)
defaultConfigPath = do
  home <- homeDir
  return $ home </> $(mkRelFile ".sbu/config.ini")

homeDir :: IO (Path Abs Dir)
homeDir =
  fromMaybe (error "error parsing home directory")
    <$> (parseAbsDir <$> getHomeDirectory)

defaultConfig :: IO Config
defaultConfig = do
  home <- homeDir
  return $ Config (home </> $(mkRelDir "sbu_backups")) 15 20

handleOptions :: SbuOptions -> IO ()
handleOptions (SbuOptions (Just configPath) command) =
  loadConfig configPath >>= handleCommand command
handleOptions (SbuOptions _ command) = defaultConfig >>= handleCommand command

handleCommand :: Command -> Config -> IO ()
handleCommand (AddCmd (AddOptions games)) _ = print games
handleCommand c conf = print $ show c ++ " " ++ show conf

loadConfig :: Path Abs File -> IO Config
loadConfig path = undefined
