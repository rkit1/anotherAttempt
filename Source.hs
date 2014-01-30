{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SourceIdentifier where
import qualified Data.Text as T
import Control.Monad.Reader

newtype LocalFP = LocalFP FilePath

class Monad m => SourceMonad m where
  toFilePath :: LocalFP -> m FilePath
  relativeToCurrent :: LocalFP -> m LocalFP
  withCurrent :: LocalFP -> m a -> m a 

data SMData = SMData 
  { root :: FilePath -- ^ should end with '/'
  , currentDir :: LocalFP -- ^ should end with '/' 
  }

newtype SM m a = SM (ReaderT SMData m a)
    deriving Monad

instance Monad m => SourceMonad (SM m) where
  toFilePath (LocalFP fp) = SM $ do
    r <- asks root
    return $ r ++ fp
  relativeToCurrent a@(LocalFP ('/':_)) = return a
  relativeToCurrent (LocalFP fp) = SM $ do
    (LocalFP cur) <- asks currentDir 
    return $ (LocalFP $ cur ++ fp)
  withCurrent (LocalFP fp) (SM m) = SM $ local (\ d -> d{currentDir = LocalFP lfp} ) m 
    where
      lfp = reverse $ dropWhile (/= '/') $ reverse fp
