{-# LANGUAGE RankNTypes #-}

import Control.Monad.Reader (runReaderT)
import Data.List (sort)
import Lib.Internal
import Pipes (runEffect, void, (>->), (>~))
import qualified Pipes.Prelude as P
import System.Directory (canonicalizePath)
import System.IO.Temp
import Test.Hspec
import Types

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

testRunConfig :: RunConfig
testRunConfig =
    RunConfig
        { runConfigConfig = testConfig
        , runConfigVerbose = False
        }

discardOutput :: Monad m => Logger m a -> m a
discardOutput p = runEffect $ p >-> P.drain

listOutput :: Monad m => Logger m a -> m [Output]
listOutput = P.toListM . void

main :: IO ()
main = hspec $ do
    describe "add game" $ do
        it "adds game to config" $ do
            path <- canonicalizePath "/new"
            result <-
                runReaderT
                    (discardOutput (addGame "new" path (Just "*")))
                    testRunConfig
            result
                `shouldBe` Just
                    ( testConfig
                        { configGames =
                            Game
                                { gameName = "new"
                                , gamePath = path
                                , gameGlob = "*"
                                } :
                            configGames testConfig
                        }
                    )

        it "prints message after adding game" $ do
            path <- canonicalizePath "/new"
            result <-
                runReaderT
                    (listOutput (addGame "new" path (Just "*")))
                    testRunConfig
            result
                `shouldBe` [ Normal "Game added successfully:\n"
                           , Normal "Name: new"
                           , Normal $ "Save path: " <> path
                           , Normal "Save glob: *"
                           , Normal ""
                           ]

    describe "list games" $ do
        it "lists all games in config" $ do
            result <- runReaderT (listOutput listGames) testRunConfig
            result `shouldBe` [Normal "another\ntest"]

    describe "print game info" $ do
        it "lists info for all games in config" $ do
            result <- runReaderT (listOutput (infoGames [])) testRunConfig
            result
                `shouldBe` [ Normal "Name: another"
                           , Normal "Save path: /another/path"
                           , Normal "Save glob: save*"
                           , Normal ""
                           , Normal "Name: test"
                           , Normal "Save path: /test/game/path"
                           , Normal ""
                           ]

        it "lists info for selected games in config" $ do
            result <-
                runReaderT
                    (listOutput (infoGames ["another"]))
                    testRunConfig
            result
                `shouldBe` [ Normal "Name: another"
                           , Normal "Save path: /another/path"
                           , Normal "Save glob: save*"
                           , Normal ""
                           ]

    describe "edit game" $ do
        it "edits game in config" $ do
            path <- canonicalizePath "/edited"
            Just Config{configGames = result} <-
                runReaderT
                    ( discardOutput
                        ( editGame
                            "test"
                            Nothing
                            (Just path)
                            (Just "none")
                        )
                    )
                    testRunConfig
            length result `shouldBe` length (configGames testConfig)
            head (filter ((== "test") . gameName) result)
                `shouldBe` Game
                    { gameName = "test"
                    , gamePath = path
                    , gameGlob = ""
                    }

        it "prints output when editing game" $ do
            path <- canonicalizePath "/edited"
            result <-
                runReaderT
                    ( listOutput
                        ( editGame
                            "another"
                            Nothing
                            (Just path)
                            (Just "none")
                        )
                    )
                    testRunConfig
            result
                `shouldBe` [ Normal "Name: another"
                           , Normal $ "Save path: /another/path -> " <> path <> ""
                           , Normal "Save glob: save* -> "
                           , Normal ""
                           ]

        it "edits game in config with new name" $ do
            Just Config{configGames = result} <-
                runReaderT
                    ( discardOutput
                        ( editGame
                            "test"
                            (Just "new")
                            Nothing
                            Nothing
                        )
                    )
                    testRunConfig
            length result `shouldBe` length (configGames testConfig)
            length (filter ((== "new") . gameName) result)
                `shouldBe` 1

        it "prints output when editing game with new name" $ do
            result <-
                runReaderT
                    ( listOutput
                        ( editGame
                            "test"
                            (Just "new")
                            Nothing
                            Nothing
                        )
                    )
                    testRunConfig
            head result `shouldBe` Normal "Name: test -> new"

    describe "remove game" $ do
        it "removes game from config" $ do
            Just Config{configGames = result} <-
                runReaderT
                    (discardOutput (return "" >~ removeGames True ["test"]))
                    testRunConfig
            length result `shouldBe` length (configGames testConfig) - 1
            length (filter ((== "test") . gameName) result) `shouldBe` 0

        it "prints output when removing game" $ do
            result <-
                runReaderT
                    (listOutput (return "y" >~ removeGames False ["another", "test"]))
                    testRunConfig
            let inner (Normal t) = t
                inner (Warning t) = t
                inner (Error t) = t
            sort (map inner result)
                `shouldBe` [ "Permanently delete another? (y/N) "
                           , "Permanently delete test? (y/N) "
                           , "Removed another"
                           , "Removed test"
                           ]

    describe "edit config" $ do
        it "edits config" $ do
            path <- canonicalizePath "/edited"
            Just result <-
                runReaderT
                    (discardOutput (editConfig path (Just 5) (Just 6)))
                    testRunConfig
            result
                `shouldBe` Config
                    { configBackupDir = path
                    , configBackupFreq = 5
                    , configBackupsToKeep = 6
                    , configGames = configGames testConfig
                    }

        it "prints output when editing config" $ do
            path <- canonicalizePath "/edited"
            result <-
                runReaderT
                    (listOutput (editConfig path (Just 5) (Just 6)))
                    testRunConfig
            result
                `shouldBe` [ Normal $ "Backup path: /backups -> " <> path
                           , Normal "Backup frequency (in minutes): 15 -> 5"
                           , Normal "Number of backups to keep: 20 -> 6"
                           , Normal ""
                           ]

    describe "save and load config" $ do
        it "works" $ do
            withSystemTempFile "config" $ \path _ -> do
                writeConfig path testConfig
                Right config <- readConfig path
                config `shouldBe` testConfig
