{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances
  , GeneralizedNewtypeDeriving, TemplateHaskell #-}
module SiteGen.DepDB where
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Language.Haskell.TH

type DepDBType si di t = (M.Map di (Either String (t, S.Set si, S.Set di)))
emptyDDBType = M.empty

newtype DepDB si di t m a = DepDB (StateT (DepDBType si di t) m a)
  deriving (Monad, MonadIO, MonadTrans, Functor, Applicative)


runDepDB :: (Monad m, Ord si, Ord di)
  => (DepDBType si di t) -> DepDB si di t m a -> m (DepDBType si di t)
runDepDB i (DepDB m) = execStateT m i

class Monad m => DepDBMonad m si di t | m -> si di t where 
  recordDeps :: di -> Either String (t, S.Set si, S.Set di) -> m ()
  lookupDeps :: di -> m (Maybe (Either String (t, S.Set si, S.Set di)))

instance (Monad m, Ord si, Ord di) => DepDBMonad (DepDB si di t m) si di t where
  recordDeps di dt = DepDB $ modify $ M.insert di dt
  lookupDeps di = DepDB $ gets $ M.lookup di

deriveDepDBMonad
  :: (Q Type -> Q Type -> Q Type -> TypeQ) -> Q [Dec]
deriveDepDBMonad mf = do
  [si, di, t] <- forM ["si", "di", "t"] $ \ n -> return . VarT <$> newName n
  [d|
    instance (DepDBMonad m $si $di $t) 
      => DepDBMonad ($(mf si di t) m) $si $di $t where
      recordDeps di dt = lift $ recordDeps di dt
      lookupDeps di = lift $ lookupDeps di 
    |]
