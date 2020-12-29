{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.Reader (runReaderT)
import Lib.Internal
import Pipes
import qualified Pipes.Prelude as P
import System.Directory (canonicalizePath)
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

discardOutput :: Monad m => Logger m a -> m a
discardOutput p = runEffect $ p >-> P.drain

listOutput :: Monad m => Logger m a -> m [String]
listOutput = P.toListM . void

main :: IO ()
main = hspec $ do
    describe "add game" $ do
        it "adds game to config" $ do
            path <- canonicalizePath "/new"
            result <-
                runReaderT
                    (discardOutput (addGame "new" path (Just "*")))
                    testConfig
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
                    testConfig
            result `shouldBe` ["Game added successfully:\n", "Name: new\nSave path: C:\\new\nSave glob: *\n"]

    describe "list games" $ do
        it "lists all games in config" $ do
            result <- runReaderT (listOutput listGames) testConfig
            result `shouldBe` ["another\ntest"]

    describe "print game info" $ do
        it "lists info for all games in config" $ do
            result <- runReaderT (listOutput (infoGames [])) testConfig
            result
                `shouldBe` [ "Name: another\nSave path: /another/path\nSave glob: save*\n"
                           , "Name: test\nSave path: /test/game/path\n"
                           ]

        it "lists info for selected games in config" $ do
            result <-
                runReaderT
                    (listOutput (infoGames ["another"]))
                    testConfig
            result `shouldBe` ["Name: another\nSave path: /another/path\nSave glob: save*\n"]

    describe "edit game" $ do
        it "edits game in config" $ do
            path <- canonicalizePath "/edited"
            Just (Config{configGames = result}) <-
                runReaderT
                    ( discardOutput
                        ( editGame
                            "test"
                            Nothing
                            (Just path)
                            (Just "none")
                        )
                    )
                    testConfig
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
                    testConfig
            result
                `shouldBe` [ "Name: another"
                           , "Save path: /another/path -> " <> path
                           , "Save glob: save* -> "
                           ]

        it "edits game in config with new name" $ do
            Just (Config{configGames = result}) <-
                runReaderT
                    ( discardOutput
                        ( editGame
                            "test"
                            (Just "new")
                            Nothing
                            Nothing
                        )
                    )
                    testConfig
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
                    testConfig
            head result `shouldBe` "Name: test -> new"

    describe "remove game" $ do
        it "removes game from config" $ do
            Just (Config{configGames = result}) <-
                runReaderT
                    (discardOutput (return "" >~ removeGames True ["test"]))
                    testConfig
            length result `shouldBe` length (configGames testConfig) - 1
            length (filter ((== "test") . gameName) result) `shouldBe` 0

        it "prints output when removing game" $ do
            result <-
                runReaderT
                    (listOutput (return "y" >~ removeGames False ["another", "test"]))
                    testConfig
            result
                `shouldBe` [ "Permanently delete test? (y/N) "
                           , "Permanently delete another? (y/N) "
                           , "Removed test"
                           , "Removed another"
                           ]

    describe "edit config" $ do
        it "edits config" $ do
            path <- canonicalizePath "/edited"
            Just result <-
                runReaderT
                    (discardOutput (editConfig path (Just 5) (Just 6)))
                    testConfig
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
                    testConfig
            result
                `shouldBe` [ "Backup path: /backups -> " <> path
                           , "Backup frequency (in minutes): 15 -> 5"
                           , "Number of backups to keep: 20 -> 6"
                           ]
