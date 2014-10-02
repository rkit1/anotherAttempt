{-# LANGUAGE StandaloneDeriving, KindSignatures, GeneralizedNewtypeDeriving
  , TemplateHaskell, DeriveDataTypeable, FlexibleContexts, FlexibleInstances
  , UndecidableInstances #-}

module ClubviRu.Config.Site where
import SiteGen.Main
import Data.Typeable
import Data.Functor
import Control.Eff
import Control.Eff.Reader.Strict

data SiteConfig = SiteConfig
  { sourceRoot_ :: FilePath
  , destionationRoot_ :: FilePath
  , storeRoot_ :: FilePath
  , myDomains_ :: [String] }
  deriving (Typeable)

class Member (Reader SiteConfig) r => HasSiteConfig r
instance Member (Reader SiteConfig) r => HasSiteConfig r

sourceRoot :: HasSiteConfig r => Eff r FilePath
sourceRoot = sourceRoot_ <$> ask

destinationRoot :: HasSiteConfig r => Eff r FilePath
destinationRoot = destionationRoot_ <$> ask

storeRoot :: HasSiteConfig r => Eff r FilePath
storeRoot = storeRoot_ <$> ask

myDomains :: HasSiteConfig r => Eff r [String]
myDomains = myDomains_ <$> ask
