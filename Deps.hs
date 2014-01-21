{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, 
  FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Deps where
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import Data.Binary (encodeFile, decodeFile, Binary)
import System.Directory
import Data.Time.Clock
import Data.Maybe
import System.IO.Unsafe

----
-- runTime myCheckTime myCurTime $ runDepDBFile "/path" $ process (runDepRecordAndReport myProcess) initD
----


----
-- main loop
----

-- | Process all destinations reachable from the initial ones. Every destination is processed only once per run.
process :: (Ord di, Monad m) 
  => (di -> m (S.Set di)) -- ^ Actual processing goes here.
                          -- Return set of destinations found 
                          -- while processing current destination.
  -> (S.Set di)           -- ^ Initial destinations.
  -> m ()
process process set = go S.empty set
  where
    go !done !queue 
     | Nothing <- S.minView queue = return ()
     | Just (dst, queueTail) <- S.minView queue = do
       ds <- process dst
       let done' = S.insert dst done
           queue' = queue `S.union` (ds `S.difference` done')
       go done' queue'   

----
-- DepRecord
----

class Monad m => DepRecordMonad m si di | m -> si di where
  recordSI :: si -> m ()
  recordDI :: di -> m ()

instance (Monad m, Ord si, Ord di) => DepRecordMonad (DepRecord si di m) si di where
  recordSI si = DepRecord $ modify $ \ (ss, dd) -> (S.insert si ss, dd)
  recordDI di = DepRecord $ modify $ \ (ss, dd) -> (ss, S.insert di dd)

newtype DepRecord si di m a = DepRecord (StateT (S.Set si, S.Set di) m a)
  deriving (Monad, MonadIO, MonadTrans)

runDepRecord :: Monad m => DepRecord si di m a -> m (S.Set si, S.Set di)
runDepRecord (DepRecord m) = execStateT m (S.empty, S.empty)

runDepRecordAndReport :: (Monad m, Ord si, Ord di, Ord t) 
  => (di -> DepRecord si di m a) 
  -> di 
  -> DepDB si di t (Time si t m) (S.Set di)
runDepRecordAndReport f di = do
  let doit = do 
        (ss, dd) <- lift $ lift $ runDepRecord (f di)
        t <- curTime 
        recordDeps di t ss dd
        return dd
  deps <- lookupDeps di
  case deps of
    Nothing -> doit
    Just (t, ss, dd) -> do
      c <- checkForChanges t ss
      if c then doit 
           else return dd

----
-- DepDB
----

newtype DepDB si di t m a = DepDB (StateT (M.Map di (t, S.Set si, S.Set di)) m a)
  deriving (Monad, MonadIO, MonadTrans)

runDepDB :: (Monad m, Ord si, Ord di) => M.Map di (t, S.Set si, S.Set di) -> DepDB si di t m a 
         -> m (M.Map di (t, S.Set si, S.Set di))
runDepDB initT (DepDB m) = execStateT m initT

-- | Run `DepDB` monad with actual database permanent storage taken care of.
runDepDBFile :: (MonadIO m, Ord si, Binary si, Ord di, Binary di, Binary t) 
  => FilePath -- ^ File is created automatically, but containing
              -- directory should exist.
  -> DepDB si di t m a -> m ()
runDepDBFile fp m = do
  !db <- liftIO $ do
    c <- doesFileExist fp
    if c 
      then decodeFile fp 
      else return M.empty
  !db' <- runDepDB db m
  liftIO $ encodeFile fp db'

class Monad m => DepDBMonad m si di t | m -> si di t where 
  recordDeps :: di -> t -> S.Set si -> S.Set di -> m ()
  lookupDeps :: di -> m (Maybe (t, S.Set si, S.Set di))

instance (Monad m, Ord si, Ord di) => DepDBMonad (DepDB si di t m) si di t where
  recordDeps di t ss dd = DepDB $ modify $ M.insert di (t, ss, dd)
  lookupDeps di = DepDB $ gets $ M.lookup di

----
-- Time
----

-- | Alias for the `getCurrentTime` to use as a `curTime`
-- 
-- You should memorize this value, since time of the start of the
-- processing is enough, so there is no need to make a lots of extra
-- syscalls.
--
-- >do
-- >  t <- curTimeUTC
-- >  let myCurTime = return t
-- >  runTime myCheckTime myCurTime $ ...
curTimeUTC :: MonadIO m => m UTCTime
curTimeUTC = liftIO $ getCurrentTime

-- | Alias for the `getModificationTime` to use as a `checkTime`
checkTimeFilePath :: MonadIO m => FilePath -> m UTCTime
checkTimeFilePath fp = liftIO $ getModificationTime fp

class Monad m => TimeMonad m si t | m -> si t where
  checkTime :: si -> m t
  curTime :: m t

instance TimeMonad m si t => TimeMonad (DepDB si di t m) si t where
  checkTime si = lift $ checkTime si
  curTime = lift $ curTime

instance (Monad m, Ord si) => TimeMonad (Time si t m) si t where
  curTime = Time $ gets curTime_ >>= \ x -> lift x
  checkTime si = Time $ do
    x <- get 
    case M.lookup si (memoMap x) of
      Just t -> return t
      Nothing -> do
        t <- lift $ checkTime_ x si
        put x{memoMap = M.insert si t $ memoMap x}
        return t
        

newtype Time si t m a = Time (StateT (TimeState si t m) m a)
  deriving (Monad, MonadIO)

runTime :: (Monad m, Ord si) => (si -> m t) -> (m t) -> Time si t m a -> m a
runTime cht cut (Time m) = evalStateT m st
  where st = TimeState
             { memoMap = M.empty
             , checkTime_ = cht
             , curTime_ = cut }

checkForChanges :: (Ord t, TimeMonad m si t) => t -> S.Set si -> m Bool
checkForChanges t ss = do
  go $ S.toList ss
  where 
    go [] = return False
    go (x:xs) = do
      st <- checkTime x
      if st > t then return True else go xs
      

instance MonadTrans (Time si t) where
  lift m = Time $ lift m

data TimeState si t m = TimeState
  { memoMap :: !(M.Map si t)
  , checkTime_ :: si -> m t
  , curTime_ :: m t }
