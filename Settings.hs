module Settings where

class SettingsMonad m where
  getSourcePath :: m FilePath
  getStoragePath :: m FilePath