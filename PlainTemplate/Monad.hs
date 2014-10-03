{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances
  , UndecidableInstances, RecordWildCards, TemplateHaskell, FlexibleContexts
  , GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeOperators
  , ScopedTypeVariables, ExistentialQuantification #-}
module PlainTemplate.Monad where
import Library
import qualified Data.Map as M
import qualified Data.Set as S
import PlainTemplate.Parser
import Text.Parsec.Pos
import SiteGen.Main
import Data.String
import Data.Typeable
import Control.Eff.Exception
import Control.Eff.State.Strict
import Control.Eff
import qualified Data.Accessor as A
import Data.Accessor.Template


newtype Template = Template Body
  deriving Typeable

data Variable = forall var. Typeable var => Variable var
  deriving Typeable

newtype Dictionary = Dictionary (M.Map String Variable)
  deriving Typeable


class ( Member (State S) r, Member (Exc Err) r
      , HasSiteIO si di t r, HasDepRecord si di r
      , Show si, IsString si )
  => HasPTL si di t r

instance ( Member (State S) r, Member (Exc Err) r
         , HasSiteIO si di t r, HasDepRecord si di r
         , Show si, IsString si  )
  => HasPTL si di t r

runPTL :: Eff (State S :> (Exc Err :> r)) a -> Eff r (Either Err a)
runPTL m = (runExc :: Eff (Exc Err :> r) a -> Eff r (Either Err a))
           $ fmap snd $ runState (S [selem])
           $ m
  where
    selem = StackElem
      { seContext__ = Dictionary M.empty
      , seTag__ = (undefined <++> id) $ head $ (undefined <++> id )
                 $ runP body () "no location" "[dummytag]"
      }


runPTLE :: Eff (State S :> (Exc Err :> r)) b -> Eff r b
runPTLE m = do
  res <- runPTL m
  case res of
    Right a -> return a
    Left err -> $terror (show err)


data Err = Err (SourcePos,SourcePos) String 
  deriving (Show, Typeable)

           
data StackElem = StackElem
  { seContext__ :: Dictionary
  , seTag__ :: Tag
  } deriving (Typeable)

data S = S
  { sStack_ :: [StackElem]
  } deriving (Typeable)

$(deriveAccessors ''StackElem)
$(deriveAccessors ''S)


throwError :: (HasPTL si di t r) => String -> Eff r b
throwError str = do
  Tag p _ _ _ <- A.getVal tag <$> get
  throwExc $ Err p str

lookupVar :: (HasPTL si di t r) => String -> Eff r Variable
lookupVar str = do
  Dictionary x <- A.getVal context <$> get 
  case M.lookup str x of
    Just a -> return a
    Nothing -> throwError $ "Variable not found: " ++ str

headA = A.accessor head (\ a (x:xs) -> a:xs ) 
context = sStack >>> headA >>> seContext_ 
tag = sStack >>> headA >>> seTag_

(%:) :: (Member (State s) r, Typeable s)
  => A.Accessor s a -> (a -> a) -> Eff r ()
a %: b = modify $ a A.^: b


setVar :: HasPTL si di t r => String -> Variable -> Eff r ()
setVar nm var = context %: \ (Dictionary a) -> Dictionary $ M.insert nm var a

updateVars :: HasPTL si di t r => Dictionary -> Eff r ()
updateVars (Dictionary a) = context %: \ (Dictionary b) -> Dictionary $ M.union a b

infixr 0 $=
($=) :: (HasPTL si di t r, Typeable var) => String -> var -> Eff r ()
($=) n v = setVar n (Variable v)

infixr 0 $=.
($=.) :: (HasPTL si di t r, Typeable var) => String -> Eff r var -> Eff r ()
a $=. b = b >>= \ b' -> a $= b'

-- variable
mcast :: forall r a si di t. (HasPTL si di t r, Typeable a) => Variable -> Eff r a
mcast (Variable a) 
  | Just x <- cast a = return x
  | otherwise = throwError $ printf "trying to cast %s to %s" (show $ typeOf a) (show $ typeOf (undefined :: a))

compareStr :: Variable -> Variable -> Ordering
compareStr (Variable a) (Variable b) = compare a' b'
  where Just a' = cast a :: Maybe String
        Just b' = cast b

mkDictionary :: [(String, Variable)] -> Dictionary
mkDictionary a = Dictionary $ M.fromList a
