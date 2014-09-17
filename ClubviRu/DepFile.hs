{-# LANGUAGE FlexibleContexts #-}
module ClubviRu.DepFile where
import ClubviRu.Resource
import SiteGen.Main
import Data.Char
import Data.String


-- | Вывод релативен!!!
readDepFile :: 
  ( DepRecordMonad m SP di
  , MonadSiteIO SP di t m) 
  => SP -> m [DP]
readDepFile si = do
  ls <- readString si
  return [ fromString l | l <- lines ls, not $ all isSpace l ]
  

