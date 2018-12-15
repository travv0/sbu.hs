module Main where

import           Options.Applicative

import           Options
import           Lib

main :: IO ()
main = handleOptions =<< execParser (info opts idm)
