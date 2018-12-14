module Main where

import           Options.Applicative

import           Options

main :: IO ()
main = print =<< execParser opts
