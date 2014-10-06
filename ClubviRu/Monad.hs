{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances
  , MultiParamTypeClasses, UndecidableInstances, OverloadedStrings, TypeFamilies
  , TemplateHaskell, TypeOperators, FlexibleContexts #-}
module ClubviRu.Monad where
import ClubviRu.Config.Site
import Network.URI
import Data.List
import Control.Monad.Trans
import SiteGen.Main
import ClubviRu.Resource
import System.IO
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad
import Data.Time
import Control.Monad.Trans.Control
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Data.Typeable


runClubviRu :: (SetMember Lift (Lift m) r, MonadIO m, Typeable m) =>
     Eff (SiteIO SP DP UTCTime :> Reader SiteConfig :> r) w -> Eff r w
runClubviRu m = runReader (loop $ admin m) config
  where
    loop (Val x) = return x
    loop (E u)   = handleRelay u loop f
      where
        f (OpenDI di@Resource{..} k) = do
          fp <- toFilePathM (di :: DP)
          dp <- toDirectoryPathM di
          h <- liftIO $ do
            createDirectoryIfMissing True dp
            h <- openFile fp WriteMode
            hSetEncoding h utf8
            return h
          loop $ k h
        f (OpenSI si k) = do
          fp <- toFilePathM (si :: SP)
          h <- liftIO $ do
            h <- openFile fp ReadMode
            hSetEncoding h utf8
            return h
          loop $ k h
        f (DoesExistSI si k) = do
          fp <- toFilePathM si
          res <- liftIO $ doesFileExist fp
          loop $ k res
        f (CopySItoDI si di k) = do
          fps <- toFilePathM si
          fpd <- toFilePathM di
          res <- liftIO $ do
            createDirectoryIfMissing True $ takeDirectory fpd
            copyFile fps fpd
          loop $ k res
        f (CheckTime si k) = do
          fp <- toFilePathM si
          res <- liftIO $ getModificationTime fp
          loop $ k res
        f (CurTime k) = liftIO getCurrentTime >>= loop . k



config :: SiteConfig
config = SiteConfig
  { sourceRoot_ = root ++ "/anotherAttemptSource/"
  , destionationRoot_ = root ++ "/anotherAttemptDestination/"
  , storeRoot_ = root ++ "/anotherAttemptStore/"
  , myDomains_ = ["clubvi.ru", "www.clubvi.ru"] }
  where
    root = "c:/Users/Victor/Documents/wrk/newsite/"
