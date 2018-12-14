module Main where

import           Options.Applicative

import           Options
import           Lib

main :: IO ()
main = handleCommand =<< execParser (info opts idm)
