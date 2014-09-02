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


test :: MonadIO m => m ()
test = do
  t <- runDepDB emptyDDBType $ runClubviRu $ runClubviTime
    $ process (runDepRecordAndReport $ \ d -> runPathHandler d clubviRoute)
    $ S.fromList ["/index.htm", "/remember.htm"]
  liftIO
    $ writeFile "c:/Users/Victor/Documents/wrk/newsite/anotherAttempt/graph"
    $ show t

-- runDepDB

