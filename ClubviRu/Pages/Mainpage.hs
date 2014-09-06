{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell
  , RecordWildCards #-}
module ClubviRu.Pages.Mainpage where
import ClubviRu.Config.Parser (parseConfigFile)
--import Path.Destination
--import Path.Source
import ClubviRu.Resource
import ClubviRu.Debug.Helpers
import Data.String
import qualified Data.Map as M
import Data.Char
import Control.Monad
import XmlTemplate.Head
import XmlTemplate.WidgetHead
import Control.Monad.Writer
import Library
import SiteGen.Deps
import qualified Data.Accessor.Monad.MTL.State as A
import qualified Data.Set as S
import qualified PlainTemplate.Variable as V
import ClubviRu.Listing
import PlainTemplate.Monad
import PlainTemplate.Process
import PlainTemplate.Variable
import SiteGen.Deps
import SiteGen.IO as IO
import ClubviRu.Config.Parser
import ClubviRu.Config.Site
import Text.Printf



(!) :: Monad m => M.Map String a -> String -> m a
a ! b = case M.lookup b a of Nothing -> error ("key not found: " ++ b)
                             Just a -> return $! a


runMainPage ::
  ( DepRecordMonad m SP DP
  , SiteConfig m, MonadSiteIO SP DP m)
  => Maybe Date -> SP -> m String
runMainPage pageDate configPath@Resource{..} = do
  Right cfg <- parseConfig `liftM` IO.readString configPath

  mid' <- cfg ! "mid"
  let mid = filter (not . all isSpace) $ lines mid'
      months = groupByMonths mid
      myChunk | Just d <- pageDate = case lookup d months of
                Nothing -> $terror "runMainPage: imposible: date not found"
                Just a -> a
              | otherwise = take 50 mid
      archiveLink = Resource
        { resPathType = Relative
        , resPath = ["archive", getName configPath]
        , resName = fromString ((showDate $ fst $ head months) ++ ".htm") }


      archiveLinkString :: String
      archiveLinkString = printf "<p><a href=\"%s\">Архив</a></p>"
        $ toFilePath "" archiveLink

  news' <- forM myChunk $ \ x -> do
    x' <- readHead $ "~head.htm.src" `relativeTo` fromString x 
    return $ mkDictionary [ ("content", Variable x' ) ]  
  let news = news' ++ [mkDictionary [ ("content", Variable archiveLinkString ) ]]

  str <- runMAndRecordSI $ do
    "title" $=. cfg ! "title"
    "leftcolumn" $=. processColumn =<< cfg ! "left"
    "rightcolumn" $=. processColumn =<< cfg ! "right"
    "news" $=. return news
    callRTPL (fromString "/~templates/mainpage1.rtpl")

  return str




runMAndRecordSI :: (MonadIO m, DepRecordMonad m SourcePath di) => M m a -> m a
runMAndRecordSI m = do
  res <- runM $ do
    a <- m
    deps <- A.get depends
    return (a, deps)
  case res of
    Right (a, deps) -> do
      forM_ (S.toList deps) $ recordSI . fromString
      return a
    Left err -> $terror (show err)


processColumn :: PTLMonad SP DP m => String -> m String
processColumn str = 
  let list = filter (not . all isSpace) $ lines str
  in liftM concat $ forM list $ \ item -> 
    case item of
      i | Just t <- stripPrefix "!widget:" i -> do
          callRTPL (fromString t)
          callRTPL "/~templates/widget.rtpl"
        | Just t <- stripPrefix "!raw:" i -> 
          IO.readString (fromString t)
        | Just t <- stripPrefix "!widgetListing:" i -> do
          Right cfg <- parseConfig `liftM` IO.readString (fromString t)
          mid' <- cfg ! "mid"
          let mid = filter (not . all isSpace) $ lines mid'
          listing <- forM mid $ \ i -> do
            str <- readWidgetHead $ "~widgetHead.htm.src" `relativeTo` fromString i
            return $ mkDictionary [ ("content", Variable str) ]
          let
            -- FIXME poor code
            link = toFilePath "" (fromString t `changeExt` "htm")
            foot = "<p><a href=\"" ++ link ++ "\">Все материалы</a></p><p></p>"
          "head" $=. cfg ! "title"
          "listing" $= listing
          "footer" $= foot
          ("body" $=) =<< callRTPL "/~templates/WidgetListing.rtpl"
          callRTPL "/~templates/widget.rtpl"
        | otherwise -> return []
--          error $ printf "processColumn: %s" i
{-
      "!banner" -> undefined
      "!remember" -> undefined -}

-- String -> M V.Dictionary


