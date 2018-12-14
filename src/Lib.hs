module Lib
  ( someFunc
  )
where

data Command
  = Backup BackupOptions
  -- | Add AddOptions
  -- | List ListOptions
  -- | Info InfoOptions
  -- | Remove RemoveOptions
  -- | Edit EditOptions
  -- | Config ConfigOptions

data BackupOptions = BackupOptions
  { games :: [String]
  , loop :: Bool
  }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
