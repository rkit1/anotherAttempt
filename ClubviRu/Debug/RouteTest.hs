{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module ClubviRu.Debog.RouteTest where
import ClubviRu.Debug.Helpers
import ClubviRu.Route
import ClubviRu.Monad
import ClubviRu.Resource
import qualified Data.Set as S
import SiteGen.Deps
import ClubviRu.Time
import Data.Time.Clock
import qualified Data.Map as M
import ClubviRu.Config.Site
import Control.Monad.Trans
import SiteGen.IO
import ClubviRu.Storage

test :: MonadIO m => m ()
test = do
  runClubviRu $ runClubviTime $ runAcidDepDB
    $ process (runDepRecordAndReport $ \ d -> do
      r <- runPathHandler d clubviRoute
      case r of
        Right _ -> return Nothing
        Left err -> liftIO $ putStrLn err >> return (Just err))
    $ S.fromList ["/index.htm", "/remember.htm"]
  return ()
