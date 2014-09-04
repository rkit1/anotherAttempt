{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies
  , DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, FlexibleContexts #-}
module ClubviRu.Storage where
import SiteGen.Deps
import ClubviRu.Config.Site
import ClubviRu.Resource
import SiteGen.IO
import Data.Time.Clock
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.Set as S

newtype AcidDepDB m a = AcidDepDB { unADD :: AcidState DB -> m a}

instance SiteConfig m => SiteConfig (AcidDepDB m) where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  storeRoot = lift storeRoot
  myDomains = lift myDomains 

instance TimeMonad m SP UTCTime => TimeMonad (AcidDepDB m) SP UTCTime where
  checkTime = lift . checkTime
  curTime = lift curTime

instance MonadSiteIO SP DP m => MonadSiteIO SP DP (AcidDepDB m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  
instance Monad m => Monad (AcidDepDB m) where
  return a = AcidDepDB $ const $ return a
  AcidDepDB f >>= g = AcidDepDB $ \ r -> do
    x <- f r
    unADD (g x) r
  AcidDepDB f >> g = AcidDepDB $ \ r -> do
    f r
    unADD g r

instance MonadTrans AcidDepDB where
  lift m = AcidDepDB $ const $ m

instance MonadIO m => MonadIO (AcidDepDB m) where
  liftIO m = lift $ liftIO m

recordDeps_ :: DP -> UTCTime -> S.Set SP -> S.Set DP -> Update DB ()
recordDeps_ di t ss dd = do
  DB a <- get
  put $ DB $ M.insert di (t, ss, dd) a
  
lookupDeps_ :: DP -> Query DB (Maybe (UTCTime, S.Set SP, S.Set DP))
lookupDeps_ di = do
  DB a <- ask
  return $ M.lookup di a
  
newtype DB = DB (DepDBType SP DP UTCTime)

instance SafeCopy Source where
  putCopy _ = undefined
  getCopy = undefined
instance SafeCopy Destination where
  putCopy _ = undefined
  getCopy = undefined

$(deriveSafeCopy 0 'base ''ResPathType)
$(deriveSafeCopy 0 'base ''Resource)
$(deriveSafeCopy 0 'base ''DB)
$(makeAcidic ''DB ['recordDeps_, 'lookupDeps_])

instance MonadIO m => DepDBMonad (AcidDepDB m) SP DP UTCTime where
  recordDeps di t ss dd = AcidDepDB $ \ r -> do
    liftIO $ update r $ RecordDeps_ di t ss dd
  lookupDeps di = AcidDepDB $ \ r -> do
    liftIO $ query r $ LookupDeps_ di


runAcidDepDB
  :: (MonadIO m, SiteConfig m)
     => AcidDepDB m b -> m b
runAcidDepDB (AcidDepDB m) = do
  path <- storeRoot
  database <- liftIO $ openLocalStateFrom (path ++ "/deps/") $ DB M.empty
  a <- m database
  liftIO $ createCheckpoint database
  liftIO $ closeAcidState database
  return a
