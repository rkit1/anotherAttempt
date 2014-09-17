{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses
 , FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances
 , UndecidableInstances, TypeFamilies, ExistentialQuantification #-}
module SiteGen.Process where
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import SiteGen.DepDB
import SiteGen.DepRecord
import SiteGen.IO

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

runDepRecordAndReport ::
  (MonadSiteIO si di t m, DepDBMonad m si a t, Ord t)
  => (a -> DepRecord si a m (Maybe String)) -> a -> m (S.Set a)
runDepRecordAndReport f di = do
  deps <- lookupDeps di
  let doit = do
        res <- runDepRecord (f di)
        t <- curTime 
        case res of
          Left err -> do
            recordDeps di $ Left err
            return S.empty
          Right (ss, dd) -> do
            recordDeps di $ Right (t, ss, dd)
            return dd
  case deps of
    Just (Right (t, ss, dd)) -> do
      c <- checkForChanges t ss
      if c then doit else return dd
    _ -> doit
