{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses
 , FunctionalDependencies, GeneralizedNewtypeDeriving, FlexibleInstances
 , UndecidableInstances, TypeFamilies, ExistentialQuantification #-}
module SiteGen.Process where
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

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
