module Lib where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Serialize (decode)
import Lib.Internal
import Options
import System.Directory (doesFileExist)
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)
import Types

handleOptions :: SbuOptions -> IO ()
handleOptions (SbuOptions configPath command) = do
    path <- fromMaybe <$> defaultConfigPath <*> pure configPath
    econfig <- readConfig path
    config <- case econfig of
        Right c -> return c
        Left _ -> do
            let backupPath = path <.> "bak"
            backupExists <- doesFileExist backupPath
            if backupExists
                then do
                    hPutStrLn
                        stderr
                        "Error reading config file, attempting to read from backup..."
                    backupConfig <- BS.readFile backupPath
                    case decode backupConfig of
                        Right c -> return c
                        Left _ -> createDefaultConfig path
                else createDefaultConfig path
    newConfig <- runSbu (handleCommand command) $ RunConfig config False
    liftIO $ maybeWriteConfig path newConfig
