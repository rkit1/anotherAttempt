{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, CPP #-}
module Path where
import Text.Parsec hiding (State)
import URL
import qualified Data.Text as T
import Prelude hiding (catch)
import Control.Exception hiding (try)
import Library
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Accessor.Template
import Data.Accessor ((^:),(^.))
import qualified Data.Accessor as A
import Data.Accessor.Monad.MTL.State ((%=),(%:))
import qualified Data.Accessor.Monad.MTL.State as A
import Library.Zipper as Z

fixPath :: String -> String
fixPath str | Right r <- t = r
            | Left err <- t = error $ printf "fixPath: trying to process %s. Error is %s" (show err) str
  where 
    t = parse p "" str 
    p = msum
      [ try (oneOf "xX" >> char ':') >> win
      , unix ]
    win = many1 (slash `mplus` anyChar)
    slash = char '\\' >> return '/'
    unix = many1 anyChar


data RoutingState = RoutingState
  { rsSource_ :: HTTPURL
  , rsPath_ :: Z.Zipper T.Text }
  deriving Show
$(deriveAccessors ''RoutingState)

newtype RoutingMonad a = RoutingMonad (MaybeT (State RoutingState) a) 
    deriving (Monad, MonadState RoutingState, MonadPlus)

segment :: RoutingMonad T.Text
segment = do
  path <- A.get rsPath
  guard $ not $ Z.endp path
  A.set rsPath $ Z.right path
  return $ Z.cursor path

matchSegment :: (T.Text -> Bool) -> RoutingMonad T.Text
matchSegment f = do
  s <- segment
  guard $ f s
  return s

pathRest :: RoutingMonad [T.Text]
pathRest = do
  Z.Zip _ r <- A.get rsPath
  return r
  
newRoutingState :: HTTPURL -> RoutingState
newRoutingState src = RoutingState src p
  where p = Z.fromList $ T.splitOn "/" $ T.pack $ path $ relativePart src

runRoutingMonad :: HTTPURL -> RoutingMonad a -> Maybe a
runRoutingMonad s m = evalState (runMaybeT f) (newRoutingState s)
  where RoutingMonad f = segment >> m





#ifdef dev
test = runRoutingMonad "http://clubvi.ru/news/2013/09/30/svirida_biyazi/" f
  where f = (liftM2 (:) segment f) `mplus` return [] 
  
#endif