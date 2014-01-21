{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, RecordWildCards, TemplateHaskell #-}
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

type M = ME (StateT S IO)

newtype ME m a = ME { runME :: m (Either E a) }

data E = E (SourcePos,SourcePos) String deriving Show

data StackElem = StackElem
  { context__ :: Variables
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

instance Monad m => Monad (ME m) where
  return a = ME ( return ( return a ) )
  ME a >>= f = ME $ do
    x <- a
    case x of 
      Left z -> return $ Left z
      Right v -> runME $ f v

instance MonadTrans ME where
  lift m = ME $ Right `liftM` m

instance MonadError String M where
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

lookupVar :: String -> M Variable
lookupVar str = do
  Variables x <- A.get context
  case M.lookup str x of
    Just a -> return a
    Nothing -> throwError $ strMsg $ "Variable not found: " ++ str

setVar :: String -> Variable -> M ()
setVar nm var = context %: \ (Variables a) -> Variables $ M.insert nm var a

updateVars :: Variables -> M ()
updateVars (Variables a) = context %: \ (Variables b) -> Variables $ M.union a b

runM :: M a -> IO (Either E a)
runM m = evalStateT ( runME m ) $ 
  S { stack_ = 
      [ StackElem
        { context__ = Variables M.empty
        , tag__ = (undefined <++> id) $ head $ (undefined <++> id ) $ runP body () "no location" "[dummytag]"
        } ]
    , depends_ = S.empty }

recordDepend :: FilePath -> M ()
recordDepend d = depends %: S.insert d

infixr 0 $=
($=) :: Typeable a => String -> a -> M ()
($=) n v = setVar n (Variable v)
infixr 0 $=.
a $=. b = b >>= \ b' -> a $= b'