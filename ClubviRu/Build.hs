{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where
import Library
import ClubviRu.Route
import ClubviRu.Monad
import ClubviRu.Resource
import qualified Data.Set as S
import SiteGen.Main
import Data.Time.Clock
import qualified Data.Map as M
import ClubviRu.Config.Site
import ClubviRu.Storage
import System.IO
import Control.Eff.Lift

main :: IO ()
main = do
  p <- runLift $ runClubviRu storeRoot -- FIXME
  runAcidDepDB p $ runClubviRu $  process (S.fromList ["/index.htm"])
    $ runDepRecordAndReport $ \ d -> do
      r <- runPathHandler d clubviRoute
      case r of
        Right _ -> return Nothing
        Left err -> liftIO $ putStrLn err >> return (Just err)

