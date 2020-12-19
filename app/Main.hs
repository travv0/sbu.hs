module Main where

import Control.Monad.Catch (catchIOError)
import Lib (handleOptions)
import Options (opts)
import Options.Applicative (execParser, idm, info)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
    (handleOptions =<< execParser (info opts idm))
        `catchIOError` \e -> hPutStrLn stderr $ "An unhandled error occurred: " ++ show e
