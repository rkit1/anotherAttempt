{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses
  , FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances
  , GeneralizedNewtypeDeriving #-}
module SiteGen.DepRecord where
import qualified Data.Set as S
import qualified Data.Map as M
import SiteGen.DepDB
import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Trans.Control
import Control.Monad.Base

class Monad m => DepRecordMonad m si di | m -> si di where
  recordSI :: si -> m ()
  recordDI :: di -> m ()

instance (Monad m, Ord si, Ord di) => DepRecordMonad (DepRecord si di m) si di where
  recordSI si = DepRecord $ modify $ \ (ss, dd) -> (S.insert si ss, dd)
  recordDI di = DepRecord $ modify $ \ (ss, dd) -> (ss, S.insert di dd)

newtype DepRecord si di m a
  = DepRecord { unDepRecord :: StateT (S.Set si, S.Set di) m a}
  deriving (Monad, MonadIO, MonadTrans, Applicative, Functor)

instance MonadBaseControl b m => MonadBaseControl b (DepRecord si di m) where
  newtype StM (DepRecord si di m) a
    = StMDepRecord {unStMDepRecord :: ComposeSt (DepRecord si di) m a}
  liftBaseWith = defaultLiftBaseWith StMDepRecord
  restoreM     = defaultRestoreM   unStMDepRecord

instance (Functor m, Monad m, MonadBase b m)
  => MonadBase b (DepRecord si di m) where
  liftBase = liftBaseDefault 

instance MonadTransControl (DepRecord si di) where
  newtype StT (DepRecord si di) a
    = StDepRecord {unStDepRecord :: StT (StateT (S.Set si, S.Set di)) a}
  liftWith = defaultLiftWith DepRecord unDepRecord StDepRecord
  restoreT = defaultRestoreT DepRecord unStDepRecord

runDepRecord :: Monad m
  => DepRecord si di m (Maybe String)
     -> m (Either String (S.Set si, S.Set di))
runDepRecord (DepRecord m) = do
  (a, sets) <- runStateT m (S.empty, S.empty)
  case a of
    Nothing ->  return $ Right (sets)
    Just err -> return $ Left err
