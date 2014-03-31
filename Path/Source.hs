{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Path.Source where
import qualified Data.Text as T
import Control.Monad.Reader
    
newtype SourcePath = SourcePath FilePath

class Monad m => SourceMonad m where
  toFilePath :: SourcePath -> m FilePath
  relativeToCurrent :: SourcePath -> m SourcePath
  withCurrent :: SourcePath -> m a -> m a 

data SMData = SMData 
  { root :: FilePath -- ^ should end with '/'
  , currentDir :: SourcePath -- ^ should end with '/' 
  }

newtype SM m a = SM (ReaderT SMData m a)
    deriving Monad

instance Monad m => SourceMonad (SM m) where
  toFilePath (SourcePath fp) = SM $ do
    r <- asks root
    return $ r ++ fp
  relativeToCurrent a@(SourcePath ('/':_)) = return a
  relativeToCurrent (SourcePath fp) = SM $ do
    (SourcePath cur) <- asks currentDir 
    return $ (SourcePath $ cur ++ fp)
  withCurrent (SourcePath fp) (SM m) = SM $ local (\ d -> d{currentDir = SourcePath lfp} ) m 
    where
      lfp = reverse $ dropWhile (/= '/') $ reverse fp
