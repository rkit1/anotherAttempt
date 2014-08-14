{-# LANGUAGE StandaloneDeriving, KindSignatures, GeneralizedNewtypeDeriving #-}

module ClubviRu.Config.Site where
import SiteGen.Deps
import Control.Monad.Trans

class Monad m => SiteConfig m where
  sourceRoot :: m FilePath
  destinationRoot :: m FilePath
  filterLinks :: [String] -> m [String]

instance (SiteConfig m) => SiteConfig (DepRecord si di m)  where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  filterLinks ls = lift $ filterLinks ls