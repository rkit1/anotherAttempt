{-# LANGUAGE FlexibleContexts #-}
module Pages.Mainpage2 where
import Config.Parser (parseConfigFile)
import LocalFP
import qualified Data.Map as M
import Data.Char
import Head
import Control.Monad.Writer
import Library
import Deps
import qualified Data.Accessor.Monad.MTL.State as A
import qualified Data.Set as S
import qualified PlainTemplate.Variable as V
import PlainTemplate.Monad
import PlainTemplate.Process
import PlainTemplate.Variable

(!) :: Monad m => M.Map String a -> String -> m a
a ! b = case M.lookup b a of Nothing -> error ("key not found: " ++ b)
                             Just a -> return $! a

{-
runMP :: FilePath 
      -> FilePath 
      -> -}
runMainPage page configPath outPath = withCurrent configPath $ do
  Right cfg -- FIXME
      <- parseConfigFile =<< toFilePath configPath 
  mid' <- cfg ! "mid"
  let mid = filter (not . all isSpace) $ lines mid'
      grouped = groupByMonths mid
      myChunk | Nothing <- page = take 50 $ mid
              | Just x <- page = undefined
  str <- runMAndRecordSI $ do
    "title" $=. cfg ! "title"
    "leftcolumn" $=. processColumn =<< cfg ! "left"
    "rightcolumn" $=. processColumn =<< cfg ! "right"
    "news" $=. flip mapM myChunk $ \ x -> do
                      str <- readHeadM x
                      return $ mkDictionary [ ("content", Variable str) ] 
    callRTPL "/~templates/mainpage1.rtpl"
--saveTo outPath str
--recordDI


runMAndRecordSI :: (MonadIO m, DepRecordMonad m FilePath di) =>
     M a -> m a
runMAndRecordSI m = do
  Right (a, deps) <- liftIO $ runM $ do
                       a <- m
                       deps <- A.get depends
                       return (a, deps)
  forM_ (S.toList deps) recordSI
  return a
  

processColumn :: String -> M String
processColumn str = 
  let list = filter (not . all isSpace) $ lines str
  in liftM concat $ forM list $ \ item -> 
    case item of
      i | Just t <- stripPrefix "widget:" i -> do
          callRTPL i
          callRTPL "/~templates/widget.rtpl"
        | Just t <- stripPrefix "raw:" i -> 
          callRTPL i
        | otherwise ->
          error $ printf "processColumn: %s" i
{-
      "!banner" -> undefined
      "!remember" -> undefined -}

-- String -> M V.Dictionary

