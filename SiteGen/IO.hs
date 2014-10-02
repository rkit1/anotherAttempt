{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances, TypeOperators
  , GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances
  , TemplateHaskell, DeriveDataTypeable, DeriveFunctor, OverlappingInstances
  , FlexibleContexts, ScopedTypeVariables, BangPatterns
  #-}
module SiteGen.IO where
import Library
import System.IO
import SiteGen.DepRecord
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Control.Applicative
import qualified Data.Set as S
import Data.Typeable
import Language.Haskell.TH
import Control.Eff
import Control.Monad.Trans
import Control.Monad

data SiteIO si di t a
  = OpenSI
    { si :: si
    , result1 :: Handle -> a }
  | OpenDI
    { di :: di
    , result2 :: Handle -> a }
  | DoesExistSI
    { si :: si
    , result3 :: Bool -> a }
  | CopySIToDI
    { si :: si
    , di :: di
    , result4 :: () -> a }
  | CheckTime
    { si :: si
    , result5 :: t -> a }
  | CurTime
    { result6 :: t -> a }
  deriving (Typeable, Functor)


--
class ( Member (SiteIO si di t) r
      , Ord si, Ord di, Ord t, Typeable  si, Typeable di, Typeable t)
      => HasSiteIO si di t r | r -> si di t

instance (Ord si, Ord di, Ord t, Typeable  si, Typeable di, Typeable t)
         => HasSiteIO si di t (SiteIO si di t :> r)

instance ( HasSiteIO si di t r
         , Ord si, Ord di, Ord t, Typeable  si, Typeable di, Typeable t)
         => HasSiteIO si di t (a :> r)      

--
openSI
  :: forall si di t r
  .  (HasSiteIO si di t r)
  => si -> Eff r Handle
openSI si = send $ \ f -> inj (t f)
  where
    t :: (Handle -> a) -> SiteIO si di t a
    t f = OpenSI si f

openDI
  :: forall si di t r
  .  (HasSiteIO si di t r)
  => di -> Eff r Handle
openDI di = send $ \ f -> inj (t f)
  where
    t :: (Handle -> a) -> SiteIO si di t a
    t f = OpenDI di f

doesExistSI
  :: forall si di t r
  .  (HasSiteIO si di t r)
  => si -> Eff r Bool
doesExistSI si = send $ \ f -> inj (t f)
  where
    t :: (Bool -> a) -> SiteIO si di t a
    t f = DoesExistSI si f

copySIToDI
  :: forall si di t r
  .  (HasSiteIO si di t r, HasDepRecord si di r)
  => si -> di -> Eff r ()
copySIToDI si di = (send $ \ f -> inj (t f)) >> recordSI si
  where
    t :: (() -> a) -> SiteIO si di t a
    t f = CopySIToDI si di f

checkTime
  :: forall si di t r
  .  (HasSiteIO si di t r)
  => si -> Eff r t
checkTime si = send $ \ f -> inj (t f)
  where
    t :: (t -> a) -> SiteIO si di t a
    t f = CheckTime si f

curTime
  :: forall si di t r
  .  (HasSiteIO si di t r)
  => Eff r t
curTime = send $ \ f -> inj (t f)
  where
    t :: (t -> a) -> SiteIO si di t a
    t f = CurTime f    
--
readString :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => si -> Eff r String
readString si = do
  recordSI si
  openSI si >>= liftIO . hGetContents

readByteString :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => si -> Eff r BS.ByteString
readByteString si = do
  recordSI si
  openSI si >>= liftIO . BS.hGetContents

readByteStringL :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => si -> Eff r LBS.ByteString
readByteStringL si = do
  recordSI si
  openSI si >>= liftIO . LBS.hGetContents

writeString :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => di -> String -> Eff r ()
writeString di str = do
  openDI di >>= liftIO . flip hPutStr str

writeByteString :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => di -> BS.ByteString -> Eff r ()
writeByteString di str = do
  openDI di >>= liftIO . flip BS.hPutStr str

writeByteStringL :: (HasDepRecord si di r, HasSiteIO si di t r, MonadIO (Eff r))
  => di -> LBS.ByteString -> Eff r ()
writeByteStringL di str = do
  openDI di >>= liftIO . flip LBS.hPutStr str


--

safeCheckTime :: HasSiteIO si di t r => si -> Eff r (Maybe t)
safeCheckTime si = do
  c <- doesExistSI si
  case c of
    True -> Just `liftM` checkTime si
    False -> return Nothing

-- | True == changed
checkForChanges :: HasSiteIO si di t r => t -> S.Set si -> Eff r Bool
checkForChanges t ss = do
  go $ S.toList ss
  where 
    go [] = return False
    go (x:xs) = do
      st <- safeCheckTime x
      case st of
        Just a | a <= t -> go xs
        _ -> return True


-- checkme
runMemoTrie
  :: forall si di t r a
  .  (HasSiteIO si di t r)
  => Eff r a -> Eff r a
runMemoTrie m = loop M.empty $ admin m
  where
    loop !m (Val x) = return x
    loop !m (E u)   = interpose u (loop m) f
      where
        f :: SiteIO si di t (VE r a) -> Eff r a
        f (CheckTime si k) = case M.lookup si m of
          Just t  -> loop m $ k t
          Nothing -> do
            t <- checkTime si
            loop (M.insert si t m) $ k t
        f _ = send (<$> u) >>= loop m

