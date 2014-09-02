{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses
 , FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances
 , UndecidableInstances #-}
module SiteGen.Deps where
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Data.Map.Strict as M

-- di = destination identifier site.com/a/b
-- si = source identifier /home/user/a/b

----
-- run everything
----
{-
runEverything
  :: (Monad m, Ord si, Ord di, Ord t) 
  => (si -> m t)                            -- ^ Check time
  -> m t                                    -- ^ Get current time
  -> (di -> DepRecord si di m a)            -- ^ Process function
  -> S.Set di                               -- ^ Initial destinations.
  -> M.Map di (t, S.Set si, S.Set di)       -- ^ Initial memo table
  -> m (M.Map di (t, S.Set si, S.Set di))   -- ^ Resulting memo table
runEverything cht cut pr initD initT = runTime cht cut $ runDepDB $ process (runDepRecordAndReport pr) initD
-}
----
-- main loop
----

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
           queue' = queueTail `S.union` (ds `S.difference` done')
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

runDepRecordAndReport
  :: (Ord t, TimeMonad m si t, DepDBMonad m si di t) =>
     (di -> DepRecord si di m a) -> di -> m (S.Set di)
runDepRecordAndReport f di = do
  deps <- lookupDeps di
  let doit = do 
        (ss, dd) <- runDepRecord (f di)
        t <- curTime 
        recordDeps di t ss dd
        return dd
  case deps of
    Nothing -> doit
    Just (t, ss, dd) -> do
      c <- checkForChanges t ss
      if c then doit else return dd

----
-- DepDB
----

type DepDBType si di t = (M.Map di (t, S.Set si, S.Set di))
emptyDDBType = M.empty

newtype DepDB si di t m a = DepDB (StateT (DepDBType si di t) m a)
  deriving (Monad, MonadIO, MonadTrans)

instance DepDBMonad m si di t => DepDBMonad (Time si t m) si di t where
  recordDeps d t ss ds = lift $ recordDeps d t ss ds
  lookupDeps d = lift $ lookupDeps d



runDepDB :: (Monad m, Ord si, Ord di)
  => (DepDBType si di t) -> DepDB si di t m a -> m (DepDBType si di t)
runDepDB i (DepDB m) = execStateT m i

class Monad m => DepDBMonad m si di t | m -> si di t where 
  recordDeps :: di -> t -> S.Set si -> S.Set di -> m ()
  lookupDeps :: di -> m (Maybe (t, S.Set si, S.Set di))

instance (Monad m, Ord si, Ord di) => DepDBMonad (DepDB si di t m) si di t where
  recordDeps di t ss dd = DepDB $ modify $ M.insert di (t, ss, dd)
  lookupDeps di = DepDB $ gets $ M.lookup di

----
-- Time
----

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
