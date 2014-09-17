{-# LANGUAGE StandaloneDeriving, KindSignatures, GeneralizedNewtypeDeriving #-}

module ClubviRu.Config.Site where
import SiteGen.Main
import Control.Monad.Trans

class Monad m => SiteConfig m where
  sourceRoot :: m FilePath
  destinationRoot :: m FilePath
  storeRoot :: m FilePath
  myDomains :: m [String]

instance (SiteConfig m) => SiteConfig (DepRecord si di m)  where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  storeRoot = lift storeRoot
  myDomains = lift myDomains
