{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances, 
  MultiParamTypeClasses, UndecidableInstances, OverloadedStrings #-}
module ClubviRu.Monad where
import ClubviRu.Config.Site
import Network.URI
import Data.List
import Control.Monad.Trans
import SiteGen.IO
import SiteGen.Deps
import ClubviRu.Resource
import System.IO
import System.Directory

newtype ClubviRuMonad m a = ClubviRuMonad {runClubviRu :: m a} 
  deriving (Monad, MonadIO)

instance (MonadIO m, DepRecordMonad m si di) => 
  DepRecordMonad (ClubviRuMonad m) si di where
    recordSI = lift . recordSI
    recordDI = lift . recordDI

instance MonadTrans ClubviRuMonad where
  lift = ClubviRuMonad



instance (MonadIO m) => 
  MonadSiteIO SourcePath DestinationPath (ClubviRuMonad m) where
    openDI di@Resource{..} = do
      fp <- toFilePathM di
      dp <- toDirectoryPathM di
      liftIO $ do
        createDirectoryIfMissing True dp
        h <- openFile fp WriteMode
        hSetEncoding h utf8
        return h
    openSI si = toFilePathM si >>= \ fp -> liftIO $ do
                  h <- openFile fp ReadMode
                  hSetEncoding h utf8
                  return h
    doesExistSI si = toFilePathM si >>= \ fp -> liftIO $ doesFileExist fp


instance Monad m => SiteConfig (ClubviRuMonad m) where
    sourceRoot = 
        return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptSource/"
    destinationRoot = 
        return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptDestination/"
    myDomains = return ["clubvi.ru", "www.clubvi.ru"]
