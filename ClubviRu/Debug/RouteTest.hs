{-# LANGUAGE OverloadedStrings #-}
module ClubviRu.Debug.MainpageTest where
import ClubviRu.Debug.Helpers
import ClubviRu.Route
import ClubviRu.Monad
import ClubviRu.Resource
import qualified Data.Set as S
import SiteGen.Deps


test :: IO ()
test = process f $ S.fromList ["/index.htm", "/remember.htm"]
  where 
    f d = do
      (ss, ds) <- runDepRecord $ runClubviRu $ runPathHandler d clubviRoute
      return ds
