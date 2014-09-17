{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module ClubviRu.Debog.RouteTest where
import ClubviRu.Debug.Helpers
import ClubviRu.Route
import ClubviRu.Monad
import ClubviRu.Resource
import qualified Data.Set as S
import SiteGen.Main
import Data.Time.Clock
import qualified Data.Map as M
import ClubviRu.Config.Site
import Control.Monad.Trans
import ClubviRu.Storage

test :: IO ()
test = do
  runClubviRu $ runAcidDepDB $ process
    ( runDepRecordAndReport $ \ d -> do
      r <- runPathHandler d clubviRoute
      case r of
        Right _ -> return Nothing
        Left err -> liftIO $ putStrLn err >> return (Just err))
    $ S.fromList ["/index.htm"]
  return ()

