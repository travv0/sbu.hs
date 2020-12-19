module Main where

import Control.Monad.Catch (catchAll)
import Lib (handleOptions)
import Options (opts)
import Options.Applicative (execParser, idm, info)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
    (handleOptions =<< execParser (info opts idm))
        `catchAll` \e -> hPutStrLn stderr $ "An unhandled error occurred: " ++ show e
