module Lib
  ( handleCommand
  )
where

import           Data.List

import           Options

handleCommand :: Command -> IO ()
handleCommand c = putStrLn $ "Command not yet implemented."
