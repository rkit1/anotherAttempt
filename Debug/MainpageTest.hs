{-# LANGUAGE OverloadedStrings #-}
module Debug.MainpageTest where
import Pages.Mainpage2
import ClubviRu.Monad
import SiteGen.Deps

test = do
  return () :: IO ()
  runDepRecord $ runClubviRu $ runMainPage 0 "/index" "/index.htm"

