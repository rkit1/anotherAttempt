{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Path.Destination where
import Control.Monad.Trans.Reader
import Control.Monad.Trans

data DestinationPath = DestinationPath [String]
  deriving Show

class DestMonad m where
  destToFilePath :: DestinationPath -> m FilePath


newtype DestT m a 
  = DestT (ReaderT DestinationPath m a)
    deriving (Monad, MonadTrans, MonadIO)

instance Monad m => DestMonad (DestT m) where
  destToFilePath (DestinationPath p) = DestT $ do
    DestinationPath basePath <- ask
    return $ concat $ normalizePath $ basePath ++ p



-- | только на абсолютных путях
normalizePath :: [String] -> [String]
normalizePath p = go p []
  where
    go ("..":xs) a = go xs $ safeTail a
    go (".":xs) a = go xs a
    go ("":[]) a = go [] ("index.htm":a)
    go ("":xs) a = go xs a
    go (x:xs) a = go xs $  x:a
    go [] a = reverse a
    safeTail [] = []
    safeTail (x:xs) = xs

parsePath p = x : case xs of 
                    [] -> []
                    _ -> parsePath $ tail xs
  where (x, xs) = span (/= '/') p
