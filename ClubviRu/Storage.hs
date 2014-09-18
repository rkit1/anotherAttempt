{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies
  , DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module ClubviRu.Storage where
import SiteGen.Main
import ClubviRu.Config.Site
import ClubviRu.Resource
import Data.Time.Clock
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data
import Control.Applicative
import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Base

newtype AcidDepDB m a = AcidDepDB { unAcidDepDB :: ReaderT (AcidState DB) m a}
  deriving (Monad, MonadTrans, MonadIO)

instance MonadBaseControl b m => MonadBaseControl b (AcidDepDB m) where
  newtype StM (AcidDepDB m) a
    = StMAcidDepDB {unStMAcidDepDB :: ComposeSt AcidDepDB m a}
  liftBaseWith = defaultLiftBaseWith StMAcidDepDB
  restoreM     = defaultRestoreM   unStMAcidDepDB

instance MonadBase b m => MonadBase b (AcidDepDB m) where
  liftBase = liftBaseDefault

instance MonadTransControl AcidDepDB where
     newtype StT AcidDepDB a = StAcidDepDB {unStAcidDepDB :: StT (ReaderT (AcidState DB)) a}
     liftWith = defaultLiftWith AcidDepDB unAcidDepDB StAcidDepDB
     restoreT = defaultRestoreT AcidDepDB unStAcidDepDB

instance SiteConfig m => SiteConfig (AcidDepDB m) where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  storeRoot = lift storeRoot
  myDomains = lift myDomains 

instance MonadSiteIO SP DP UTCTime m
  => MonadSiteIO SP DP UTCTime (AcidDepDB m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  checkTime = lift . checkTime
  curTime = lift curTime
  
instance Monad m => Functor (AcidDepDB m) where
  fmap = liftM

instance Monad m => Applicative (AcidDepDB m) where
  pure = return
  a <*> b = do
    f <- a
    arg <- b
    return $ f arg

recordDeps_ :: DP -> Either String (UTCTime, S.Set SP, S.Set DP) -> Update DB ()
recordDeps_ di dt = do
  DB a <- get
  put $ DB $ M.insert di dt a
  
lookupDeps_ :: DP -> Query DB (Maybe (Either String (UTCTime, S.Set SP, S.Set DP)))
lookupDeps_ di = do
  DB a <- ask
  return $ M.lookup di a

dumpDB :: Query DB DB
dumpDB = ask

newtype DB = DB (DepDBType SP DP UTCTime)
  deriving (Typeable, Show)

instance SafeCopy Source where
  putCopy _ = undefined
  getCopy = undefined
instance SafeCopy Destination where
  putCopy _ = undefined
  getCopy = undefined

$(deriveSafeCopy 0 'base ''ResPathType)
$(deriveSafeCopy 0 'base ''Resource)
$(deriveSafeCopy 0 'base ''DB)
$(makeAcidic ''DB ['recordDeps_, 'lookupDeps_, 'dumpDB])

instance MonadIO m => DepDBMonad (AcidDepDB m) SP DP UTCTime where
  recordDeps di dt = AcidDepDB $ do
    r <- ask
    liftIO $ update r $ RecordDeps_ di dt
  lookupDeps di = AcidDepDB $ do
    r <- ask
    liftIO $ query r $ LookupDeps_ di


runAcidDepDB :: (MonadIO m, SiteConfig m, MonadBaseControl IO m)
  => AcidDepDB m b -> m b
runAcidDepDB (AcidDepDB (ReaderT m)) = do
  path <- storeRoot
  liftBaseOp
    (bracket
      (openLocalStateFrom (path ++ "/deps/") $ DB M.empty)
      (\ db -> createCheckpoint db >> closeAcidState db))
    m


dumpStorage :: IO ()
dumpStorage = do
  let path = "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptStore/deps/"
  db <- openLocalStateFrom path $ DB M.empty
  d <- query db DumpDB
  closeAcidState db
  writeFile "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptStore/dump"
    $ show d

returnStorage :: IO (DepDBType SP DP UTCTime)
returnStorage = do
  let path = "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptStore/deps/"
  db <- openLocalStateFrom path $ DB M.empty
  DB d <- query db DumpDB
  closeAcidState db
  return d
  
