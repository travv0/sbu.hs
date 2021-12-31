{-# LANGUAGE RankNTypes #-}

import Lib.Internal (readConfig, writeConfig)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types (
    Config (..),
    Game (Game, gameGlob, gameName, gamePath),
 )

testConfig :: Config
testConfig =
    Config
        { configBackupDir = "/backups"
        , configBackupFreq = 15
        , configBackupsToKeep = 20
        , configGames =
            [ Game
                { gameName = "test"
                , gamePath = "/test/game/path"
                , gameGlob = ""
                }
            , Game
                { gameName = "another"
                , gamePath = "/another/path"
                , gameGlob = "save*"
                }
            ]
        }

main :: IO ()
main = hspec $ do
    describe "save and load config" $ do
        it "works" $ do
            withSystemTempFile "config" $ \path _ -> do
                writeConfig path testConfig
                Right config <- readConfig path
                config `shouldBe` testConfig
