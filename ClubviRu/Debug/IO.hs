{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction, TemplateHaskell #-}
module ClubviRu.Debug.IO where
import Debug.Helpers
import SiteGen.IO 
import SiteGen.Deps
import System.IO
import ClubviRu.Path
import ClubviRu.Config
import Control.Monad.Trans
import Config.Site
import Pages.Mainpage2

newtype DebugIO m a = DebugIO {runDebugIO :: m a}
  deriving (Monad, MonadIO, SiteConfig)


instance (MonadIO m, SiteConfig m) => 
  MonadSiteIO SourcePath DestinationPath (DebugIO m) where
    openDI di = liftIO $ do
      print di
      return stdout
    openSI si = toFilePath si >>= \ fp -> liftIO $ openFile fp ReadMode
    

instance Monad m => DepRecordMonad (DebugIO m) SourcePath DestinationPath  where
  recordSI si = return ()
  recordDI di = return ()


test :: IO ()
test = runClubviRu $ runDebugIO $ runMainPage 0 "/index" "."