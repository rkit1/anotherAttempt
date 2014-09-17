{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses
  , FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances
  , GeneralizedNewtypeDeriving #-}
module SiteGen.DepRecord where
import qualified Data.Set as S
import qualified Data.Map as M
import SiteGen.DepDB
import Control.Monad.State.Strict
import Control.Applicative
import SiteGen.Time

class Monad m => DepRecordMonad m si di | m -> si di where
  recordSI :: si -> m ()
  recordDI :: di -> m ()

instance (Monad m, Ord si, Ord di) => DepRecordMonad (DepRecord si di m) si di where
  recordSI si = DepRecord $ modify $ \ (ss, dd) -> (S.insert si ss, dd)
  recordDI di = DepRecord $ modify $ \ (ss, dd) -> (ss, S.insert di dd)

newtype DepRecord si di m a = DepRecord (StateT (S.Set si, S.Set di) m a)
  deriving (Monad, MonadIO, MonadTrans, Applicative, Functor)

runDepRecord :: Monad m
  => DepRecord si di m (Maybe String)
     -> m (Either String (S.Set si, S.Set di))
runDepRecord (DepRecord m) = do
  (a, sets) <- runStateT m (S.empty, S.empty)
  case a of
    Nothing ->  return $ Right (sets)
    Just err -> return $ Left err

runDepRecordAndReport
  :: (Ord t, TimeMonad m si t, DepDBMonad m si di t) =>
     (di -> DepRecord si di m (Maybe String)) -> di -> m (S.Set di)
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
