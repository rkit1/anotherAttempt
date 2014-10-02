{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, TypeOperators
 , FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances
 , UndecidableInstances, TypeFamilies, ExistentialQuantification #-}
module SiteGen.Process where
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import SiteGen.DepDB
import SiteGen.DepRecord
import SiteGen.IO
import Control.Eff

process :: (Ord di, Monad m) 
  => (S.Set di)           -- ^ Initial destinations.
  -> (di -> m (S.Set di)) -- ^ Actual processing goes here.
                          -- Return set of destinations found 
                          -- while processing current destination.

  -> m ()
process set process = go S.empty set
  where
    go !done !queue 
     | Nothing <- S.minView queue = return ()
     | Just (dst, queueTail) <- S.minView queue = do
       ds <- process dst
       let done' = S.insert dst done
           queue' = queueTail `S.union` (ds `S.difference` done')
       go done' queue'



runDepRecordAndReport :: (HasDepDB si di t r, HasSiteIO si di t r)
  => (di -> Eff (DepRecord si di :> r) (Maybe String))
  -> di -> Eff r (S.Set di)
runDepRecordAndReport f di = lookupDeps di >>= go
  where
    go (Just (Right (t, ss, dd))) = do
      c <- checkForChanges t ss
      if c then doit else return dd
    go _ = doit
    
    doit = do
      ((ss, dd), res) <- runDepRecord (f di)
      t <- curTime 
      case res of
       Just err -> do
         recordDeps di $ Left err
         return S.empty
       Nothing  -> do
         recordDeps di $ Right (t, ss, dd)
         return dd

