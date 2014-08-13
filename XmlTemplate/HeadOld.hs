{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}
module XmlTemplate.Head where

--import MainMonad
import XmlTemplate.Monad
import XmlTemplate.Imgs
import qualified Data.ByteString as BS
import Text.XmlHtml
import Library
import Library.System
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as BZ
import qualified Data.ByteString as BS
import ClubviRu.Path
import Debug.Trace
import System.FilePath as FP

-- fixme
fixPath = undefined

-- путь до директории
processHead path = withHead onNodes path

-- полный путь
processHead2 path = withHead2 onNodes path

withHead onNodes path = do
  s <- liftIO $ BS.readFile ("x:/" ++ path ++ "/~head.htm.src")
  withSubst ("path", [TextNode $ T.pack path]) $ do
    let Right HtmlDocument{..} = parseHTML path s
    onNodes docContent

withHead2 onNodes path = do
  s <- liftIO $ BS.readFile ("x:" ++ path)
  withSubst ("path", [TextNode $ T.pack (FP.takeDirectory  path)]) $ do 
    let Right HtmlDocument{..} = parseHTML path s
    onNodes docContent

processHeadFile :: FilePath -> IO ()
processHeadFile fp' = do
  let fp = fixPath fp'
  (_,out) <- runMT M.empty (M.fromList [imgsTag]) $ processHead fp
  h <- openFile (fp' ++ "/~head.htm") WriteMode
  BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR out
  hClose h

processHeadFile2 :: FilePath -> FilePath -> IO ()
processHeadFile2 sfp fp = do
  (_,out) <- runMT M.empty (M.fromList [imgsTag]) $ processHead2 sfp
  h <- openBinaryFile ("x:/" ++ fp) WriteMode
  BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR out
  hClose h
