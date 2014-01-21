{-# LANGUAGE RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, ExistentialQuantification, DeriveDataTypeable #-}
module PlainTemplate.NeedRebuild 
--    ( updateRebuildInfo
--    , isNeedRebuild
    where
import qualified Store as S
import Data.Binary 
--import Data.Binary.Generic.Extensions
import Control.Monad.Trans.Maybe
import System.Directory
import Library
import Library.System
import Data.Data
import PlainTemplate.Listing
--import Data.Time.Clock
    
data Depends
  = Depends
    { dTime :: UTCTime
    , dFiles :: [FilePath]
    , dListing :: [FilePath] }
    deriving (Typeable)


instance Binary UTCTime where
  put t = put $ show t
  get = liftM read get


instance Binary Depends where
  put Depends{..} = do
    put dTime
    put dFiles
    put dListing
  get = do
    dTime <- get
    dFiles <- get
    dListing <- get
    return Depends{..}

$(S.make "x:/~store/" "needRebuild" [t|Depends|])

isNeedRebuild :: FilePath -> [FilePath] -> IO Bool
isNeedRebuild fp list = do
  a <- S.lookup needRebuild fp
  case a of
    Nothing -> return True
    Just Depends{..} -> go dFiles
      where 
       go [] = return $ dListing /= list
       go (x:xs) = do
         t <- safeGetModificationTime ("x:" ++ x)
         case t of
           Just a | a <= dTime -> go xs
           _ -> return True
           
updateRebuildInfo :: FilePath -> [FilePath] -> [FilePath] -> IO ()
updateRebuildInfo fp dListing dFiles = do
  dTime <- getModificationTime fp
  S.insert needRebuild fp Depends{..}