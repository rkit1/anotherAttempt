{-# LANGUAGE OverloadedStrings #-}
module ClubviRu.Debug.MainpageTest where
import ClubviRu.Pages.Mainpage
import ClubviRu.Monad
import SiteGen.Deps

test = do
  return () :: IO ()
  runDepRecord $ runClubviRu $ runMainPage 0 "/index.mp" "/index.htm"

