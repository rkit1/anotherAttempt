{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances
  , GeneralizedNewtypeDeriving, TypeOperators, FlexibleContexts
  , UndecidableInstances, OverlappingInstances, DeriveDataTypeable, DeriveFunctor #-}
module SiteGen.DepDB where
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Eff
import Control.Applicative
import Data.Typeable

type DepDBRecord si di t = (Either String (t, S.Set si, S.Set di))

data DepDB si di t a
  = RecordDeps di (DepDBRecord si di t) a
  | LookupDeps di (Maybe (DepDBRecord si di t) -> a)
    deriving (Typeable, Functor)

--
class ( Member (DepDB si di t) r
      , Ord si, Ord di, Typeable  si, Typeable di, Typeable t)
      => HasDepDB si di t r | r -> si di t

instance (Ord si, Ord di, Typeable  si, Typeable di, Typeable t)
         => HasDepDB si di t (DepDB si di t :> r)

instance ( HasDepDB si di t r
         , Ord si, Ord di, Typeable  si, Typeable di, Typeable t)
         => HasDepDB si di t (a :> r)
--

runDepDB
  :: (Typeable si, Typeable di, Typeable t, Ord di, Ord si)
  => Eff (DepDB si di t :> r) a -> Eff r a
runDepDB m = loop M.empty $ admin m
  where
    loop m (Val x) = return x
    loop m (E u)   = handleRelay u (loop m) f
      where
        f (RecordDeps di dat k) = loop (M.insert di dat m) k
        f (LookupDeps di k)     = loop m (k $ M.lookup di m)

--

recordDeps
  :: (HasDepDB si di t r)
  => di -> DepDBRecord si di t -> Eff r ()
recordDeps di re = send $ \ f ->
  inj (RecordDeps di re $ f ())

lookupDeps
  :: (HasDepDB si di t r)
  => di -> Eff r (Maybe (DepDBRecord si di t))
lookupDeps di = send $ \ f ->
  inj (LookupDeps di f)
