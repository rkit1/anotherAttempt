{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, BangPatterns
  , FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances
  , GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts
  , DeriveDataTypeable, ScopedTypeVariables, TypeOperators
  , DeriveFunctor, OverlappingInstances  #-}
module SiteGen.DepRecord where
import Control.Eff
import Control.Eff.Writer.Strict
import qualified Data.Set as S
import Data.Typeable
import Data.Monoid
import Data.Functor

data DepRecord si di v = DepRecord (Either si di) v
  deriving (Typeable, Functor)

--
class ( Member (DepRecord si di) r
      , Ord si, Ord di, Typeable  si, Typeable di)
      => HasDepRecord si di r | r -> si di

instance (Ord si, Ord di, Typeable  si, Typeable di)
         => HasDepRecord si di (DepRecord si di :> t)

instance ( HasDepRecord si di r
         , Ord si, Ord di, Typeable  si, Typeable di)
         => HasDepRecord si di (t :> r)

--

runDepRecord :: (Typeable di, Typeable si, Ord di, Ord si)
  => Eff (DepRecord si di :> r) a -> Eff r ((S.Set si, S.Set di), a)
runDepRecord m = loop (S.empty, S.empty) $ admin m
  where
    loop st@(!ss, !ds) (Val x) = return (st, x)
    loop st@(!ss, !ds) (E u)   = handleRelay u (loop st) f
      where
        f (DepRecord i k)
          | Left  si <- i = loop (si `S.insert` ss, ds) k
          | Right di <- i = loop (ss, di `S.insert` ds) k


peek :: forall si di r a. (HasDepRecord si di r)
  => Eff r a -> Eff r a
peek m = loop $ admin m
  where
    loop (Val x) = return x
    loop (E u) =  interpose u loop f
      where
        f :: DepRecord si di (VE r a) -> Eff r a
        f (DepRecord _ k) = loop k

--

recordSI :: forall si di r.
  (HasDepRecord si di r) => si -> Eff r ()
recordSI si = send $ \ f ->
  inj (DepRecord (Left si :: Either si di) $ f ())


recordDI :: forall si di r.
  (HasDepRecord si di r) => di -> Eff r ()   
recordDI di = send $ \ f ->
  inj (DepRecord (Right di :: Either si di) $ f ())


{-
test :: ((S.Set String, S.Set Int), ())
test = run $ runDepRecord $ do
  recordSI "asd"
  recordDI 123
  peek $ recordDI 321
  recordSI "dsa"  

-}
