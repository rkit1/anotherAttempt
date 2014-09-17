{-# LANGUAGE TemplateHaskell #-}
module ClubviRu.Time where
import SiteGen.Main
import ClubviRu.Resource
import ClubviRu.Config.Site
import Control.Monad.Trans
import System.Directory
import ClubviRu.Debug.Helpers
import Data.Time

runClubviTime :: (SiteConfig m, MonadIO m) => Time SP UTCTime m a -> m a
runClubviTime = runTime cht cut
  where
    cht si = toFilePathM si >>= \ fp -> liftIO $ getModificationTime fp
    cut = liftIO $ getCurrentTime
