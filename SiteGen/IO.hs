{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances, GeneralizedNewtypeDeriving, FunctionalDependencies #-}
module SiteGen.IO where
import System.IO
import Control.Monad.Reader
import SiteGen.Deps
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

class MonadIO m => MonadSiteIO si di m | m -> si di where
  openSI :: si -> m Handle
  openDI :: di -> m Handle
  doesExistSI :: si -> m Bool
  copySI :: si -> di -> m ()

instance MonadSiteIO si di m => MonadSiteIO si di (DepRecord si di m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  copySI si di = lift $ copySI si di


------
{-
newtype SIO si di t a = SIO (ReaderT (SIOData si di t) t a) deriving (Monad, MonadIO)
instance MonadTrans (SIO si di) where
  lift m = SIO $ lift m

data SIOData si di m = SIOData
  { _openSI :: si -> m Handle
  , _openDI :: di -> m Handle } 

instance (MonadIO t) => MonadSiteIO si di (SIO si di t) where
  openSI si = SIO $ ask >>= \ SIOData{..} -> lift $ _openSI si
  openDI di = SIO $ ask >>= \ SIOData{..} -> lift $ _openDI di

runSIO :: (si -> m Handle) -> (di -> m Handle) -> SIO si di m a -> m a
runSIO _openSI _openDI (SIO m) = runReaderT m $ SIOData{..}
-}

readString :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => si -> m String
readString si = do
  recordSI si
  openSI si >>= liftIO . hGetContents

readByteString :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => si -> m BS.ByteString
readByteString si = do
  recordSI si
  openSI si >>= liftIO . BS.hGetContents

readByteStringL :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => si -> m LBS.ByteString
readByteStringL si = do
  recordSI si
  openSI si >>= liftIO . LBS.hGetContents

writeString :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => di -> String -> m ()
writeString di str = do
  openDI di >>= liftIO . flip hPutStr str

writeByteString :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => di -> BS.ByteString -> m ()
writeByteString di str = do
  openDI di >>= liftIO . flip BS.hPutStr str

writeByteStringL :: (DepRecordMonad m si di, MonadSiteIO si di m) 
  => di -> LBS.ByteString -> m ()
writeByteStringL di str = do
  openDI di >>= liftIO . flip LBS.hPutStr str