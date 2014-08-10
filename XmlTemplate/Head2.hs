{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
module XmlTemplate.Head2 where

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
import Path
import SiteGen.Deps
import Data.Knob

readHead :: (DepRecordMonad m SourcePath di, MonadSiteIO SourcePath di m) =>
     SourcePath -> m String
readHead sp = do
  -- CHECKME
  let (SP spdir) = "." `relativeTo` sp
      path = toFilePath' "" spdir
  s <- IO.readByteString sp
  (_,nodes) <- runMT M.empty (M.fromList [imgsTag]) $ do
                    withSubst ("path", [TextNode $ T.pack path ]) $ do 
                      let Right HtmlDocument{..} = parseHTML (show sp) s
                      onNodes docContent
  liftIO $ do
    knob <- newKnob (BS.pack [])
    h <- newFileHandle knob "readHeadKnob" ReadWriteMode
    BZ.toByteStringIO (BS.hPutStr h) $ nodesToBLDR nodes
    hSeek h AbsoluteSeek 1 
    out <- hGetContents h
    last out `seq` hClose h
    return out


