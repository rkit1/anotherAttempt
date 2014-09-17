{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances
  , UndecidableInstances, RecordWildCards, TemplateHaskell, FlexibleContexts
  , GeneralizedNewtypeDeriving #-}
module PlainTemplate.Monad where
import Library
import qualified Data.Map as M
import qualified Data.Set as S
import PlainTemplate.Variable
import PlainTemplate.Parser
import Control.Monad.State.Strict
import Control.Monad.Error
import Text.Parsec.Pos
import qualified Data.Accessor as A
import Data.Accessor.Monad.MTL.State ((%=),(%:))
import qualified Data.Accessor.Monad.MTL.State as A
import Data.Accessor.Template
import Control.Exception as E
import SiteGen.Main
import Data.String

class (MonadState S m, MonadIO m, MonadError String m, Show si, Show di
      , MonadSiteIO si di m, DepRecordMonad m si di, IsString si, IsString di)
      => PTLMonad si di m
      
instance (MonadState S m, MonadIO m, MonadError String m, Show si, Show di
         , MonadSiteIO si di m, DepRecordMonad m si di, IsString si, IsString di)
         => PTLMonad si di m

newtype ME m a = ME { runME :: m (Either E a) }





data E = E (SourcePos,SourcePos) String deriving Show

data StackElem = StackElem
  { context__ :: Dictionary
  , tag__ :: Tag
  } 

data S = S
  { stack_ :: [StackElem]
  , depends_ :: S.Set FilePath
  }

$(deriveAccessors ''StackElem)
$(deriveAccessors ''S)
headA = A.accessor head (\ a (x:xs) -> a:xs ) 
context = stack >>> headA >>> context_ 
tag = stack >>> headA >>> tag_

instance Monad m => Functor (ME m) where
  fmap = liftM

instance Monad m => Applicative (ME m) where
  pure = return
  a <*> b = do
    f <- a
    arg <- b
    return $ f arg

instance Monad m => Monad (ME m) where
  return a = ME ( return ( return a ) )
  ME a >>= f = ME $ do
    x <- a
    case x of 
      Left z -> return $ Left z
      Right v -> runME $ f v

instance MonadTrans ME where
  lift m = ME $ Right `liftM` m

instance Monad m => MonadError String (ME (StateT S m)) where
  throwError str = ME $ do
    Tag p _ _ _ <- A.get tag
    return $ Left $ E p str 
  catchError m catcher = ME $ do
    x <- runME m
    case x of 
      Left (E p str) -> runME $ catcher str
      Right val -> return $ Right val

instance MonadState s m => MonadState s (ME m) where
    get = lift get
    put s = lift $ put s

instance (Error a, MonadError a (ME m), MonadIO m) => MonadIO (ME m) where
  liftIO a = do
    x <- lift $ liftIO $ E.try a
    case x of
      Left e -> throwError $ strMsg $ show (e :: SomeException)
      Right z -> return z

lookupVar :: PTLMonad si di m => String -> m Variable
lookupVar str = do
  Dictionary x <- A.get context
  case M.lookup str x of
    Just a -> return a
    Nothing -> throwError $ strMsg $ "Variable not found: " ++ str

setVar :: PTLMonad si di m => String -> Variable -> m ()
setVar nm var = context %: \ (Dictionary a) -> Dictionary $ M.insert nm var a

updateVars :: PTLMonad si di m => Dictionary -> m ()
updateVars (Dictionary a) = context %: \ (Dictionary b) -> Dictionary $ M.union a b

recordDepend :: PTLMonad si di m => FilePath -> m ()
recordDepend d = depends %: S.insert d

infixr 0 $=
($=) :: (Typeable a, PTLMonad si di m) => String -> a -> m ()
($=) n v = setVar n (Variable v)


infixr 0 $=.
($=.) :: (Typeable a, PTLMonad si di  m)
  => String -> m a -> m ()
a $=. b = b >>= \ b' -> a $= b'

newtype M m a = M {runM' :: ME (StateT S m) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadIO, MonadState S)

instance MonadSiteIO si di m => MonadSiteIO si di (M m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  
instance DepRecordMonad m si di => DepRecordMonad (M m) si di where
  recordSI = lift . recordSI
  recordDI = lift . recordDI

instance MonadTrans M where
  lift m = M $ lift $ lift m 

runM :: Monad m => M m a -> m (Either E a)
runM (M m) = evalStateT ( runME m ) $ 
  S { stack_ = 
      [ StackElem
        { context__ = Dictionary M.empty
        , tag__ = (undefined <++> id) $ head $ (undefined <++> id )
                  $ runP body () "no location" "[dummytag]"
        } ]
    , depends_ = S.empty }
