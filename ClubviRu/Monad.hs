{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances, 
  MultiParamTypeClasses, UndecidableInstances, OverloadedStrings, TypeFamilies #-}
module ClubviRu.Monad where
import ClubviRu.Config.Site
import Network.URI
import Data.List
import Control.Monad.Trans
import SiteGen.Main
import ClubviRu.Resource
import System.IO
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad
import Data.Time
import Control.Monad.Trans.Control

newtype ClubviRuMonad m a = ClubviRuMonad {runClubviRu :: m a} 
  deriving (Monad, MonadIO, Functor, Applicative)

instance (MonadIO m, DepRecordMonad m si di) => 
  DepRecordMonad (ClubviRuMonad m) si di where
    recordSI = lift . recordSI
    recordDI = lift . recordDI


instance DepDBMonad m si di t => DepDBMonad (ClubviRuMonad m) si di t where
  recordDeps d dt = lift $ recordDeps d dt
  lookupDeps d = lift $ lookupDeps d


instance MonadTrans ClubviRuMonad where
  lift = ClubviRuMonad

instance (Functor m, Monad m, MonadBase b m)
  => MonadBase b (ClubviRuMonad m) where
  liftBase = liftBaseDefault 

instance MonadBaseControl b m => MonadBaseControl b (ClubviRuMonad m) where
  newtype StM (ClubviRuMonad m) a =
    StMClubvi {unStMClubvi :: ComposeSt ClubviRuMonad m a}
  liftBaseWith = defaultLiftBaseWith StMClubvi
  restoreM     = defaultRestoreM   unStMClubvi

instance MonadTransControl ClubviRuMonad where
  newtype StT ClubviRuMonad a = StClubvi {unStClubvi :: a}
  liftWith f = ClubviRuMonad $ f $ liftM StClubvi . runClubviRu
  restoreT = ClubviRuMonad . liftM unStClubvi


instance (MonadIO m) => 
  MonadSiteIO SourcePath DestinationPath UTCTime (ClubviRuMonad m) where
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
    copySItoDI si di = do
      fps <- toFilePathM si
      fpd <- toFilePathM di
      liftIO $ do
        createDirectoryIfMissing True $ takeDirectory fpd
        copyFile fps fpd
    checkTime si = toFilePathM si >>= \ fp -> liftIO $ getModificationTime fp
    curTime = liftIO $ getCurrentTime 

instance Monad m => SiteConfig (ClubviRuMonad m) where
  sourceRoot = 
    return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptSource/"
  destinationRoot = 
    return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptDestination/"
  storeRoot = 
    return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptStore/"
  myDomains = return ["clubvi.ru", "www.clubvi.ru"]
