{-# LANGUAGE RecordWildCards, ViewPatterns, FlexibleContexts #-}
module Head where
import Library.System
--import Config
import System.Process
import System.IO.Unsafe
import Text.Printf
--import Cache
import XmlTemplate.Head
import XmlTemplate.WidgetHead 
import Library
import Debug.Trace
import SiteGen.Deps
import PlainTemplate.Monad

prefix = "x:"

intro path = x ++ printf "<?$d=\"%s\";?>" path
  where
    x = unsafePerformIO (readFileE utf8 "x:/~templates/intro.php")

readHeadM :: FilePath -> M String
readHeadM path = do
  recordDepend path
  readHead path


readHeadAndRecordSI :: (MonadIO m, DepRecordMonad m FilePath di) => FilePath -> m String
readHeadAndRecordSI path = do
  recordSI (prefix ++ path ++ "/~head.htm")
  readHead path

readHead :: MonadIO m => String -> m String
readHead path = liftIO $ do
  let fp = prefix ++ path ++ "/~head.htm"
  whenNotM (doesFileExist fp) $ processHeadFile (prefix ++ path)
  forceReadFileE utf8 fp


--readHead :: String -> IO String
--readHead path = do
--  c <- doesFileExist ("x:/" ++ path ++ "/~head.htm")
--  if c then forceReadFileE utf8 ("x:/" ++ path ++ "/~head.htm")
--       else cacheOP (processHeadFile2 (path ++ "/~head.htm.src"))
--                    (forceReadFileE utf8 . ("x:/"++))
--                    (path ++ "/~head.htm.src")
  
readWidgetHead :: String -> IO String 
readWidgetHead path = do
  let fp = prefix ++ path ++ "/~WidgetHead.htm"
  whenNotM (doesFileExist fp) $ processWidgetHeadFile (prefix ++ path)
  forceReadFileE utf8 fp

---------------
-- на удаление
---------------
--pintro path = x ++ dpart path
--  where
--    x = unsafePerformIO (readFileE utf8 "x:/~templates/pintro.php")
--readHead path = forceReadFileE utf8 $ prefix ++ path ++ "~head.htm"

--p path = do
--  f <- forceReadFileE utf8 $ prefix ++ path ++ "/~head.htm"
--  readProcessE utf8 "c:/php/php.exe" [] $ pintro path ++ f

