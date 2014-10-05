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

data Eplus constr = Eplus
  { eplus_ :: forall r a. constr r => Eff r a -> Eff r a -> Eff r a
  , ezero_ :: forall r a. constr r => Eff r a }
  deriving (Typeable)

class (Typeable eplus, Member (Reader eplus) r) => EplusClass eplus r | r -> eplus

instance EplusClass eplus r => EplusClass eplus (a :> r)

instance (Typeable constr) =>
         EplusClass (Eplus constr) ((Reader (Eplus constr)) :> r) 


instance SetMember Eplus (Reader (Eplus constr)) ((Reader (Eplus constr)) :> r)

ezero :: forall r a constr.
  (EplusClass (Eplus constr) r, constr r) =>
  Eff r a
ezero = do
  Eplus{ezero_ = a} <- ask :: Eff r (Eplus constr)
  a

eplus :: forall r a constr.
  (EplusClass (Eplus constr) r, constr r) =>
  Eff r a -> Eff r a -> Eff r a 
eplus a b = do
  Eplus{eplus_ = f} <- ask :: Eff r (Eplus constr)
  f a b

esum :: (constr r, EplusClass (Eplus constr) r) =>
        [Eff r a] -> Eff r a
esum (x:xs) = x `eplus` esum xs
esum [] = ezero

runEplusExc :: forall e r a. (Typeable e) =>
  e -> Eff (Reader (Eplus (Member (Exc e))) :> r) a -> Eff r a
runEplusExc zero m = runReader m ep
  where
    ep :: Eplus (Member (Exc e))
    ep = Eplus eplus ezero
    ezero :: forall r a. (Member (Exc e)) r => Eff r a
    ezero = throwExc zero
    eplus :: forall r a. (Member (Exc e)) r =>
             Eff r a -> Eff r a -> Eff r a
    eplus a b = catchExc a f
      where
        f :: e -> Eff r a
        f _ = b


test :: Either String [String]
test = run $ runExc $ runEplusExc "ezero" $ do
  a <- ezero `eplus` return "asd"
  b <- return "dsa" `eplus` return "zcx"
  return [a, b]
