{-# LANGUAGE FlexibleInstances, ExistentialQuantification, DeriveDataTypeable, ScopedTypeVariables, PatternGuards #-}
module PlainTemplate.Variable 
    ( Template(..)
    , Variable(..)
    , mcast
    , Dictionary(..)
    , mkDictionary
    , Typeable(..)
    , compareStr
    ) where

import Data.Typeable
import qualified Data.Map as M
import Control.Monad.Error.Class
import PlainTemplate.Parser
import Text.Printf

mcast :: forall m a err. (Error err, MonadError err m, Typeable a) => Variable -> m a
mcast (Variable a) 
  | Just x <- cast a = return x
  | otherwise = throwError $ strMsg 
      $ printf "trying to cast %s to %s" (show $ typeOf a) (show $ typeOf (undefined :: a))

compareStr :: Variable -> Variable -> Ordering
compareStr (Variable a) (Variable b) = compare a' b'
  where Just a' = cast a :: Maybe String
        Just b' = cast b

newtype Template = Template Body
  deriving Typeable

data Variable = forall var. Typeable var => Variable var
  deriving Typeable

newtype Dictionary = Dictionary (M.Map String Variable)
  deriving Typeable

mkDictionary :: [(String, Variable)] -> Dictionary
mkDictionary a = Dictionary $ M.fromList a