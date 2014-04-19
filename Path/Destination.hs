{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Path.Destination where
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import System.IO
import Network.URL


data DestinationPath = DestinationPath [String]
  deriving Show


-- FIXME deal with absoultes
uRLtoDestinationPath :: URL -> Either String DestinationPath
uRLtoDestinationPath u@URL{..} 
  | Absolute _ <- url_type = Left $ "uRLtoDestinationPath: absolute URL: \""++ exportURL u  ++"\""
  | otherwise  = Right $ DestinationPath $ go url_path id
  where
    go ('/':xs) c = (c []) : go xs id
    go (x:xs) c = go xs (c . (x:))
    go [] c = [c []]
  

class DestMonad m where
  destToFilePath :: DestinationPath -> m FilePath
  destToAbsolute :: DestinationPath -> m DestinationPath


newtype DestT m a 
  = DestT (ReaderT DestinationPath m a)
    deriving (Monad, MonadTrans, MonadIO)

instance Monad m => DestMonad (DestT m) where
  destToFilePath p = do
    DestinationPath a <- destToAbsolute p
    return $ concat a
  destToAbsolute (DestinationPath p) = DestT $ do
    DestinationPath basePath <- ask
    return $ DestinationPath $ normalizePath $ basePath ++ p

saveTo :: (MonadIO m, DestMonad m) => DestinationPath -> String -> m ()
saveTo dfp str = do
  fp <- destToFilePath dfp
  liftIO $ writeFile fp str
  

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
