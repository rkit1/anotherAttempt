{-# LANGUAGE ScopedTypeVariables, BangPatterns, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Deps where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.State.Strict
import Data.Binary
import Control.Monad.Trans

class (Ord di, Binary di) => DestinationID di
class (Ord si, Binary si) => SourceID si
instance DestinationID FilePath
instance SourceID FilePath

-- | `Data.Binary.encodeFile` reimported for conviniece
encodeFile :: (Binary a, MonadIO m) => FilePath -> a -> m ()
encodeFile fp a = liftIO $ Data.Binary.encodeFile fp a

-- | `Data.Binary.decodeFile` reimported for conviniece
decodeFile :: (Binary a, MonadIO m) => FilePath -> m a
decodeFile fp = liftIO $ Data.Binary.decodeFile fp

-- | class for 'TrackMonad'
class (Monad m, SourceID si, DestinationID di) => MonadTrack si di m | m -> si di where
  trackSI :: si -> m ()
  trackDI :: di -> m ()

-- | simple monad to keep track of dependencies 
--
-- > process (\ di -> runTrackMonad $ do ... ) ...
--
-- `process` (convenience link)
newtype TrackMonad si di m a = TrackMonad (StateT (S.Set si, S.Set di) m a)
  deriving (MonadTrans, Monad, MonadIO)

-- | self-explanatory
runTrackMonad :: (SourceID si, DestinationID di, Monad m) => (TrackMonad si di m a) -> m (S.Set si, S.Set di)
runTrackMonad (TrackMonad m) = execStateT m (S.empty, S.empty)

instance (SourceID si, DestinationID di, Monad m) => MonadTrack si di (TrackMonad si di m) where 
  trackSI si = TrackMonad $ modify $ \ (ss, dd) -> (S.insert si ss, dd)
  trackDI di = TrackMonad $ modify $ \ (ss, dd) -> (ss, S.insert di dd)


-- | helper for FilePath as DestinationID
readSouceFP :: (MonadIO m, DestinationID di) => FilePath -> TrackMonad FilePath di m String
readSouceFP fp = do
  trackSI fp
  liftIO $ readFile fp

-- | helper for FilePath as SourceID
saveDestinationFP :: (MonadIO m, SourceID si) => FilePath -> String -> TrackMonad si FilePath m ()
saveDestinationFP fp str = do
  trackDI fp
  liftIO $ writeFile fp str

-- | Transofrms `M.Map` produced by `process` into map required by `checkChanges`
reverseDepMap :: forall a b. (Ord a, Ord b) => M.Map a (S.Set b) -> M.Map b (S.Set a)
reverseDepMap map = go M.empty $ M.toList map 
  where 
    go :: M.Map b (S.Set a) -> [(a, S.Set b)] -> M.Map b (S.Set a)
    go !resultMap [] = resultMap
    go !resultMap ((k, v):xs) = go resultMap' xs
      where
        resultMap' = foldl (flip $ M.alter alter) resultMap $ S.toList v
        alter Nothing = Just $ S.singleton k
        alter (Just a) = Just $ S.insert k a
      
-- | > oldMap `mergeDepMaps` newMap
mergeDepMaps :: (DestinationID di, SourceID si, Monad m) 
  => M.Map di (S.Set si) -> M.Map di (S.Set si) -> M.Map di (S.Set si)
mergeDepMaps = flip M.union

process :: forall di si m. (DestinationID di, SourceID si, Monad m) 
  => (di -> m (S.Set di)) -- ^ Actual processing goes here.
                          -- Return set of destinations found 
                          -- while processing current destination.
  -> (S.Set di)           -- ^ Initial destinations.
  -> m ()
process process set = go M.empty set
  where
    go :: M.Map di (S.Set si) -> S.Set di -> m (M.Map di (S.Set si))
    go !done !queue 
     | Nothing <- S.minView queue = return resultMap
     | Just (dst, queueTail) <- S.minView queue = do
       (ss, ds) <- process dst
       let queue' = foldl (flip S.insert) queue 
                      [ x | x <- S.toList ds, not $ x `M.member` resultMap' ]
           resultMap' = M.insert dst ss resultMap
       go resultMap' queue'   
