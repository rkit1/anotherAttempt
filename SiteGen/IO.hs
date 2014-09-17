{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances #-}
module SiteGen.IO where
import System.IO
import Control.Monad.Reader
import SiteGen.DepRecord
import SiteGen.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

class MonadIO m => MonadSiteIO si di m | m -> si di where
  openSI :: si -> m Handle
  openDI :: di -> m Handle
  doesExistSI :: si -> m Bool

instance MonadSiteIO si di m => MonadSiteIO si di (DepRecord si di m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI

instance MonadSiteIO si di m => MonadSiteIO si di (Time si t m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI

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
