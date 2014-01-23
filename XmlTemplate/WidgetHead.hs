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
import Path

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
