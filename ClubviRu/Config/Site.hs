{-# LANGUAGE StandaloneDeriving, KindSignatures, GeneralizedNewtypeDeriving #-}

module ClubviRu.Config.Site where
import SiteGen.Deps
import Control.Monad.Trans

class Monad m => SiteConfig m where
  sourceRoot :: m FilePath
  destinationRoot :: m FilePath
  myDomains :: m [String]

instance (SiteConfig m) => SiteConfig (DepRecord si di m)  where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  myDomains = lift myDomains

instance (SiteConfig m) => SiteConfig (Time si t m) where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  myDomains = lift myDomains
