{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts, KindSignatures
  , AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses
  , DeriveDataTypeable, DeriveFunctor, Rank2Types, ConstraintKinds
  , FunctionalDependencies, UndecidableInstances, StandaloneDeriving, PolyKinds
  , OverlappingInstances #-}
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Reader.Strict
import Data.Typeable

deriving instance Typeable Member

data Eplus constr a
  = GetEplus ((Ezero_ constr, Eplus_ constr) -> a)
    deriving (Typeable, Functor)

data Ezero_ constr = Ezero_ (forall r a. constr r => Eff r a)
data Eplus_ constr = Eplus_ (forall r a. constr r => Eff r a -> Eff r a -> Eff r a)

runEplus :: forall constr r a.
  (Typeable constr, constr r) =>
  (Ezero_ constr) -> 
  (Eplus_ constr) -> 
  Eff (Eplus constr :> r) a -> Eff r a
runEplus ezero eplus m = loop $ admin m
  where
    loop (Val x) = return x
    loop (E u)   = handleRelay u loop (f . th)
      where
        th :: forall a. Eplus constr a -> Eplus constr a
        th a = a
        f (GetEplus k) = loop $ k (ezero, eplus)


class (Typeable eplus, Member eplus r) =>
      EplusClass (eplus :: * -> *) r | r -> eplus

instance EplusClass eplus r => EplusClass eplus (a :> r)

instance Typeable constr => EplusClass (Eplus constr) ((Eplus constr) :> r)

askEplus :: EplusClass (Eplus constr) r => Eff r (Ezero_ constr, Eplus_ constr)
askEplus = do
  send (inj . GetEplus)


ezero :: forall r a constr.
  (EplusClass (Eplus constr) r, constr r) =>
  Eff r a
ezero = do
  (Ezero_ z, _) <- askEplus
  z

eplus :: forall r a constr.
  (EplusClass (Eplus constr) r, constr r) =>
  Eff r a -> Eff r a -> Eff r a 
eplus a b = do
  (_, Eplus_ f) <- askEplus
  f a b


esum :: (constr r, EplusClass (Eplus constr) r) =>
        [Eff r a] -> Eff r a
esum (x:xs) = x `eplus` esum xs
esum [] = ezero


runEplusExc :: forall e r a.
  (Member (Exc e) r, Typeable e) =>
  e -> Eff (Eplus (Member (Exc e)) :> r) a -> Eff r a
runEplusExc zero m = runEplus (Ezero_ z) (Eplus_ p) m
  where
    z :: forall r a. Member (Exc e) r => Eff r a
    z = throwExc zero
    p :: forall r a. Member (Exc e) r => Eff r a -> Eff r a -> Eff r a
    p a b = catchExc a f
      where
        f :: e -> Eff r a
        f _ = b


test :: [Either String String]
test = map ( \ m -> run $ runExc $ runEplusExc "ezero" m)
       [ ezero `eplus` return "asd"
       , return "dsa" `eplus` return "zcx"
       , ezero `eplus` ezero ]
