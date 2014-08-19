{-# LANGUAGE OverloadedStrings #-}
module ClubviRu.Debug.MainpageTest where
import ClubviRu.Route
import ClubviRu.Monad
import qualified Data.Set as S
import SiteGen.Deps


test :: IO ()
test = process f $ S.singleton "/index.htm"
  where 
    f d = do
      (ss, ds) <- runDepRecord $ runClubviRu $ runPathHandler d clubviRoute
      return ds
