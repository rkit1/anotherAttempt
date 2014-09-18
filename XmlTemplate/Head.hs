{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings, FlexibleContexts
  , TemplateHaskell #-}
module XmlTemplate.Head where

--import MainMonad
import XmlTemplate.Monad
import XmlTemplate.Imgs
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder as BZ
import Text.XmlHtml
import qualified Data.Text as T
import qualified Data.Map as M
import SiteGen.IO as IO
import System.IO
import Control.Monad.Trans
import ClubviRu.Resource
import SiteGen.Main
import Data.Knob
import Library

readHead :: (DepRecordMonad m SP di, MonadSiteIO SP di t m) => SP -> m String
readHead src = do
  let path = pathToString src
  s <- IO.readByteString src
  (_,nodes) <- runMT M.empty (M.fromList [imgsTag]) $ do
                    withSubst ("path", [TextNode $ T.pack path ]) $ do 
                      let Right HtmlDocument{..} = parseHTML (show src) s
                      onNodes docContent
  liftIO $ do
    knob <- newKnob (BS.pack [])
    h <- newFileHandle knob "readHeadKnob" ReadWriteMode
    BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR nodes
    hSeek h AbsoluteSeek 0
    hSetEncoding h utf8
    out <- hGetContents h
    last out `seq` hClose h
    return out

readHeadU :: (DepRecordMonad m SP di, MonadSiteIO SP di t m)
  => SP -> m String
readHeadU src = do
  let head = "~head.htm.src" `relativeTo` src
      headL = "~head.htm" `relativeTo` src
  h <- doesExistSI head
  if h
    then readHead head
    else do
      hl <- doesExistSI headL
      if hl
        then IO.readString headL
        else $terror (pathToString src ++ ": no head file")
