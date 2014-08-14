{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances, 
  MultiParamTypeClasses, UndecidableInstances, OverloadedStrings #-}
module ClubviRu.Monad where
import ClubviRu.Config.Site
import Network.URI
import Data.List
import Control.Monad.Trans
import SiteGen.IO
import SiteGen.Deps
import ClubviRu.Path
import System.IO

newtype ClubviRuMonad m a = ClubviRuMonad {runClubviRu :: m a} 
  deriving (Monad, MonadIO)

instance (MonadIO m, DepRecordMonad m si di) => 
  DepRecordMonad (ClubviRuMonad m) si di where
    recordSI = lift . recordSI
    recordDI = lift . recordDI

instance MonadTrans ClubviRuMonad where
  lift = ClubviRuMonad



instance (MonadIO m) => 
  MonadSiteIO SourcePath DestinationPath (ClubviRuMonad m) where
    openDI di = toFilePath di >>= \ fp -> liftIO $ do
                  h <- openFile fp WriteMode
                  hSetEncoding h utf8
                  return h
    openSI si = toFilePath si >>= \ fp -> liftIO $ do
                  h <- openFile fp ReadMode
                  hSetEncoding h utf8
                  return h


instance Monad m => SiteConfig (ClubviRuMonad m) where
    sourceRoot = 
        return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptSource/"
    destinationRoot = 
        return "c:/Users/Victor/Documents/wrk/newsite/anotherAttemptDestination/"
    filterLinks links = return $ links >>= f . parseURIReference
      where
        f Nothing = [] -- FIXME warning
        f (Just URI{..}) = case uriAuthority of
           Nothing -> [uriPath]
           Just URIAuth{..} 
             | Nothing <- find (== uriRegName) domainList -> []
             | uriPath == "" -> ["/"++uriPath]
             | otherwise     -> [uriPath]
        domainList = ["clubvi.ru", "www.clubvi.ru"]
