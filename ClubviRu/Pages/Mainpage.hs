{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell, TypeOperators
  , RecordWildCards, ScopedTypeVariables #-}
module ClubviRu.Pages.Mainpage where
import ClubviRu.Config.Parser (parseConfigFile)
--import Path.Destination
--import Path.Source
import ClubviRu.Resource
import Data.String
import qualified Data.Map as M
import Data.Char
import Control.Monad
import ClubviRu.Pages.Mainpage.Head
import ClubviRu.Pages.Mainpage.WidgetHead
import Control.Monad.Writer
import Library
import SiteGen.Main
import qualified Data.Accessor.Monad.MTL.State as A
import qualified Data.Set as S
import ClubviRu.Date
import PlainTemplate.Monad
import PlainTemplate.Process
import SiteGen.IO as IO
import ClubviRu.Config.Parser
import ClubviRu.Config.Site
import Text.Printf
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Exception

--(!) :: Monad m => M.Map String a -> String -> m a
a ! b = case M.lookup b a of Nothing -> $terror ("key not found: " ++ b)
                             Just a -> return $! a


runMainPage ::
  ( HasDepRecord SP DP r, HasSiteConfig r, HasSiteIO SP DP t r
  , SetMember Lift (Lift m) r, MonadIO m, Typeable m) =>
  Maybe Date -> SP -> Eff r String
runMainPage pageDate mpFile = do
  Right cfg <- readConfig mpFile

  mid <- cfg ! "mid"
  months <- peekMonths mpFile mid
  myChunk <- case pageDate of
    Nothing -> queryLines mpFile mid 50
    Just d -> queryMonth mpFile mid d
  let archiveLink m = Resource
        { resPathType = Absolute
        , resPath = "archive" : resPath mpFile ++ [getName mpFile]
        , resName = fromString (showDate m ++ ".htm") }


      archiveLinkString :: DP -> String -> String
      archiveLinkString l n = printf "<p><a href=\"%s\">%s</a></p>"
        (toFilePath "" l) n 

  news' <- forM myChunk $ \ x -> do
    x' <- readHeadU x 
    return $ mkDictionary [ ("content", Variable x' ) ]  
  let news = news' ++ [ mkDictionary [ ("content", Variable archiveMainLink ) ]
                      | pageDate == Nothing ]
      archiveMainLink = archiveLinkString (archiveLink $ head months) "Архив"


  runPTLE $ do
    let
      right
        | Nothing <- pageDate = cfg ! "right" >>= processColumn
        | Just d <- pageDate = do
          "head" $= ("Архив" :: String)
          "body" $= concat
            [ archiveLinkString (archiveLink m) (showDate m)
            | m <- months ]
          callRTPL "/~templates/widget.rtpl"

    "title" $=. cfg ! "title"
    "leftcolumn" $=. processColumn =<< cfg ! "left"
    "rightcolumn" $=. right
    "news" $= news
    callRTPL "/~templates/mainpage1.rtpl"


runMainPageListing ::
  ( HasDepRecord SP DP r, HasSiteIO SP DP t r
  , SetMember Lift (Lift m) r, MonadIO m, Typeable m) 
  => SP -> Eff r String
runMainPageListing mplFile = do
  Right cfg <- readConfig mplFile

  mid' <- cfg ! "mid"
  let mid = filter (not . all isSpace) $ lines mid'

  links <- forM mid $ \ x -> do
    let link = fromString x `relativeTo` mplFile
    Right pcfg <- readConfig link
    title <- pcfg ! "title"
    let linkStr = printf "<p><a href=\"%s\">%s</a></p>"
                  (toFilePath "" $ link `changeExt` "htm")
                  title :: String
    return $ mkDictionary [ ("content", Variable linkStr ) ]

  runPTLE $ do
    "title" $=. cfg ! "title"
    "leftcolumn" $=. processColumn =<< cfg ! "left"
    "rightcolumn" $=. processColumn =<< cfg ! "right"
    "news" $= links
    callRTPL "/~templates/mainpage1.rtpl"
  


processColumn :: (HasPTL SP DP t r) => String -> Eff r String
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


