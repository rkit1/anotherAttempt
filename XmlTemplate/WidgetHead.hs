{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}
module XmlTemplate.WidgetHead where

--import MainMonad
import XmlTemplate.Monad
import XmlTemplate.WidgetHeadImg
import qualified Data.ByteString as BS
import Text.XmlHtml
import Library
import Library.System
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as BZ
import qualified Data.ByteString as BS
import ClubviRu.Path

processWidgetHeadFile = undefined
--import PlainTemplate.Monad
{-
readHeadM :: FilePath -> M String
readHeadM path = do
  recordDepend path
  readHead path
-}


{-
readHeadAndRecordSI :: (MonadIO m, DepRecordMonad m FilePath di) => FilePath -> m String
readHeadAndRecordSI path = do
  recordSI (prefix ++ path ++ "/~head.htm")
  readHead path

readHead :: MonadIO m => String -> m String
readHead path = liftIO $ do
  let fp = prefix ++ path ++ "/~head.htm"
  whenNotM (doesFileExist fp) $ processHeadFile (prefix ++ path)
  forceReadFileE utf8 fp


  
readWidgetHead :: String -> IO String 
readWidgetHead path = do
  let fp = prefix ++ path ++ "/~WidgetHead.htm"
  whenNotM (doesFileExist fp) $ processWidgetHeadFile (prefix ++ path)
  forceReadFileE utf8 fp

-}


{-
prefix = "x:"

intro path = x ++ printf "<?$d=\"%s\";?>" path
  where
    x = unsafePerformIO (readFileE utf8 "x:/~templates/intro.php")
-}