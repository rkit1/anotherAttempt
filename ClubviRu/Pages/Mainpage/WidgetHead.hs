{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings, TemplateHaskell
  , FlexibleContexts #-}
module ClubviRu.Pages.Mainpage.WidgetHead where

--import MainMonad
import XmlTemplate
import ClubviRu.Pages.Mainpage.WidgetHeadImg
import qualified Data.ByteString as BS
import Text.XmlHtml
import Library
import Library.System
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as BZ
import qualified Data.ByteString as BS
import SiteGen.Main
import ClubviRu.Resource
import qualified Data.Map as M
import Data.Knob
import Control.Eff
import Control.Eff.Exception

readWidgetHead
  :: (HasDepRecord SP DP r, HasSiteIO SP DP t r)
  => SP -> Eff r String
readWidgetHead path@Resource{..} = do
--  c <- doesExistSI path
--  unless c $ throwExc 
--    $ (printf "readWidgetHead: %s does not exist" (toFilePath "" path) :: String)
  s <- readByteString path
  (_,nodes) <- case parseXML (show path) s of
   Left x -> $terror x
   Right XmlDocument{..} -> runMT M.empty (M.fromList [wImgTag]) $ do
     withSubst ("path", [TextNode $ T.pack (pathToString path)])
       $ onNodes docContent  
  liftIO $ do
    knob <- newKnob (BS.pack [])
    h <- newFileHandle knob "readHeadKnob" ReadWriteMode
    BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR nodes
    hSeek h AbsoluteSeek 0
    hSetEncoding h utf8
    out <- hGetContents h
    last out `seq` hClose h
    return out


{-
processWidgetHead path = withWidgetHead onNodes path

withWidgetHead onNodes path = do
  s <- liftIO $ BS.readFile ("x:/" ++ path ++ "/~WidgetHead.htm.src")
  withSubst ("path", [TextNode $ T.pack path]) $ do
    case parseXML path s of
      Right XmlDocument{..} -> onNodes docContent
      Left x -> error x

processWidgetHeadFile fp' = do
  let fp = fixPath fp'
  (_,out) <- runMT M.empty (M.fromList [wImgTag]) $ processWidgetHead fp
  h <- openFile (fp' ++ "/~WidgetHead.htm") WriteMode
  BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR out
  hClose h
-}
