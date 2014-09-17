{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses
  , FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances
  , GeneralizedNewtypeDeriving #-}
module SiteGen.Time where
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.State.Strict
import Control.Applicative
import Unsafe.Coerce
import SiteGen.DepDB
import qualified Data.Set as S
import qualified Data.Map as M

class Monad m => TimeMonad m si t | m -> si t where
  checkTime :: si -> m t
  curTime :: m t

instance TimeMonad m si t => TimeMonad (DepDB si di t m) si t where
  checkTime si = lift $ checkTime si
  curTime = lift curTime

instance (Ord si, MonadIO m) => TimeMonad (Time si t m) si t where
  curTime = Time $ gets curTime_ >>= lift
  checkTime si = Time $ do
    x <- get 
    case M.lookup si (memoMap x) of
      Just t -> return t
      Nothing -> do
        t <- lift $ checkTime_ x si
        put x{memoMap = M.insert si t $ memoMap x}
        return t

newtype Time si t m a = Time (StateT (TimeState si t m) m a)
  deriving (Monad, MonadIO, Functor, Applicative)

instance MonadTransControl (Time si t) where
  data StT (Time si t) a = forall m. StTime (a, TimeState si t m)
  liftWith f = Time $ StateT $ \s -> do
    x <- f $ \ (Time t) -> do
      state <- runStateT t $ unsafeCoerce s
      return $ StTime state
    return (x,  s)

  restoreT m = do
    Time $ StateT $ \ _ -> do
      StTime a <- m
      return $ unsafeCoerce a

instance MonadBaseControl b m => MonadBaseControl b (Time si t m) where
  newtype StM (Time si t m) a = StMT {unStMT :: ComposeSt (Time si t) m a}
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT
  

instance (Functor m, Monad m, MonadBase b m)
  => MonadBase b (Time si t m) where
  liftBase = liftBaseDefault 

runTime :: (MonadIO m, Ord si) => (si -> m t) -> (m t) -> Time si t m a -> m a
runTime cht cut (Time m) = 
  evalStateT m TimeState
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
  

instance DepDBMonad m si di t => DepDBMonad (Time si t m) si di t where
  recordDeps d dt = lift $ recordDeps d dt
  lookupDeps d = lift $ lookupDeps d
