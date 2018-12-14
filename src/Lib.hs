module Lib
  ( handleCommand
  )
where

import           Data.List

import           Options

handleCommand :: SbuOptions -> IO ()
handleCommand = print
