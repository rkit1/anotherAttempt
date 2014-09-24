{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances #-}
module SiteGen.IO where
import System.IO
import Control.Monad.Reader
import SiteGen.DepRecord
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Applicative
import qualified Data.Set as S
import Control.Monad.Trans.Maybe

class MonadIO m => MonadSiteIO si di t m | m -> si di t where
  openSI :: si -> m Handle
  openDI :: di -> m Handle
  doesExistSI :: si -> m Bool
  copySItoDI :: si -> di -> m ()
  checkTime :: si -> m t
  curTime :: m t
  

instance MonadSiteIO si di t m => MonadSiteIO si di t (DepRecord si di m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  copySItoDI si di = lift $ copySItoDI si di
  checkTime = lift . checkTime
  curTime = lift curTime

instance MonadSiteIO si di t m => MonadSiteIO si di t (Peek si di m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  copySItoDI si di = lift $ copySItoDI si di
  checkTime = lift . checkTime
  curTime = lift curTime

readString :: (DepRecordMonad m si di, MonadSiteIO si di t m) 
  => si -> m String
readString si = do
  recordSI si
  openSI si >>= liftIO . hGetContents

readByteString :: (DepRecordMonad m si di, MonadSiteIO si di t m) 
  => si -> m BS.ByteString
readByteString si = do
  recordSI si
  openSI si >>= liftIO . BS.hGetContents

readByteStringL :: (DepRecordMonad m si di, MonadSiteIO si di t m) 
  => si -> m LBS.ByteString
readByteStringL si = do
  recordSI si
  openSI si >>= liftIO . LBS.hGetContents

writeString :: (DepRecordMonad m si di, MonadSiteIO si di t m)
  => di -> String -> m ()
writeString di str = do
  openDI di >>= liftIO . flip hPutStr str

writeByteString :: (DepRecordMonad m si di, MonadSiteIO si di t m) 
  => di -> BS.ByteString -> m ()
writeByteString di str = do
  openDI di >>= liftIO . flip BS.hPutStr str

writeByteStringL :: (DepRecordMonad m si di, MonadSiteIO si di t m) 
  => di -> LBS.ByteString -> m ()
writeByteStringL di str = do
  openDI di >>= liftIO . flip LBS.hPutStr str


newtype MemoTime si t m a = MemoTime (StateT (M.Map si t) m a)
  deriving (Monad, MonadIO, Functor, Applicative, MonadTrans)

instance (Ord si, MonadSiteIO si di t m)
  => MonadSiteIO si di t (MemoTime si t m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  copySItoDI si di = lift $ copySItoDI si di
  curTime = lift curTime
  checkTime si = MemoTime $ do
    x <- get 
    case M.lookup si x of
      Just t -> return t
      Nothing -> do
        t <- lift $ checkTime si
        modify $ M.insert si t
        return t


safeCheckTime :: (MonadSiteIO si di t m) => si -> m (Maybe t)
safeCheckTime si = do
  c <- doesExistSI si
  case c of
    True -> Just `liftM` checkTime si
    False -> return Nothing


runMemoTime :: (MonadIO m, Ord si) => MemoTime si t m a -> m a
runMemoTime (MemoTime m) = evalStateT m M.empty


-- | True == changed
checkForChanges:: (MonadSiteIO si di t m, Ord t) => t -> S.Set si -> m Bool
checkForChanges t ss = do
  go $ S.toList ss
  where 
    go [] = return False
    go (x:xs) = do
      st <- safeCheckTime x
      case st of
        Just a | a <= t -> go xs
        _ -> return True
