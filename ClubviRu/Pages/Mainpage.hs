{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}
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
import Control.Monad.Writer
import Library
import SiteGen.Deps
import qualified Data.Accessor.Monad.MTL.State as A
import qualified Data.Set as S
import qualified PlainTemplate.Variable as V
import PlainTemplate.Listing
import PlainTemplate.Monad
import PlainTemplate.Process
import PlainTemplate.Variable
import SiteGen.LinkExtractor
import SiteGen.Deps
import SiteGen.IO as IO
import ClubviRu.Config.Parser
import ClubviRu.Config.Site
import ClubviRu.URIParser


(!) :: Monad m => M.Map String a -> String -> m a
a ! b = case M.lookup b a of Nothing -> error ("key not found: " ++ b)
                             Just a -> return $! a



runMainPage
  :: (IsString di, SiteConfig m, DepRecordMonad m SourcePath di,
      MonadSiteIO SourcePath di m) =>
     Int -> SourcePath -> di -> m ()
runMainPage pageNumber configPath outPath = do
  Right cfg <- parseConfig `liftM` IO.readString configPath

  mid' <- cfg ! "mid"
  let mid = filter (not . all isSpace) $ lines mid'
      myChunk = take 50 $ drop (50*pageNumber) mid

  news <- forM myChunk $ \ x -> do
    x' <- readHead $ "~head.htm.src" `relativeTo` fromString x 
    return $ mkDictionary [ ("content", Variable x' ) ]  

  str <- runMAndRecordSI $ do
    "title" $=. cfg ! "title"
    "leftcolumn" $=. processColumn =<< cfg ! "left"
    "rightcolumn" $=. processColumn =<< cfg ! "right"
    "news" $=. return news
    callRTPL "/~templates/mainpage1.rtpl"

  links <- filterLinks $ extractLinkStrings str
  forM_ links $ \ l -> recordDI $ fromString l

  writeString outPath str





runMAndRecordSI :: (MonadIO m, DepRecordMonad m SourcePath di) => M a -> m a
runMAndRecordSI m = do
  res <- liftIO $ runM $ do
    a <- m
    deps <- A.get depends
    return (a, deps)
  case res of
    Right (a, deps) -> do
      forM_ (S.toList deps) $ recordSI . fromString
      return a
    Left err -> $terror (show err)


processColumn :: PTLMonad m => String -> m String
processColumn str = 
  let list = filter (not . all isSpace) $ lines str
  in liftM concat $ forM list $ \ item -> 
    case item of
      i | Just t <- stripPrefix "widget:" i -> do
          callRTPL t
          callRTPL "/~templates/widget.rtpl"
        | Just t <- stripPrefix "raw:" i -> 
          callRTPL t
        | otherwise -> return []
--          error $ printf "processColumn: %s" i
{-
      "!banner" -> undefined
      "!remember" -> undefined -}

-- String -> M V.Dictionary


