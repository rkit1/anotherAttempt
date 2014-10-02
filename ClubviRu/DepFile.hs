{-# LANGUAGE FlexibleContexts #-}
module ClubviRu.DepFile where
import ClubviRu.Resource
import SiteGen.Main
import Data.Char
import Data.String
import Control.Eff
import Control.Monad.Trans

-- | Вывод релативен!!!
readDepFile :: 
  ( HasDepRecord SP di r
  , HasSiteIO SP di t r ) 
  => SP -> Eff r [DP]
readDepFile si = do
  ls <- readString si
  return [ fromString l | l <- lines ls, not $ all isSpace l ]
  

