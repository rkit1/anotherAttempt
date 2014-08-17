{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving,
  RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
  NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables  #-}
module ClubviRu.Route where
import ClubviRu.Config.Site
import ClubviRu.Resource
import ClubviRu.Debug.Helpers
import ClubviRu.Pages.Mainpage
import System.Directory
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error
import SiteGen.IO
import SiteGen.Deps
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T


----
newtype PathHandler m a =
  PH { runPH :: ErrorT String (StateT (Resource Destination) m) a }
    deriving (Monad, MonadPlus, MonadIO)

instance MonadTrans PathHandler where
  lift m = PH $ lift $ lift m

instance MonadSiteIO si di m => MonadSiteIO si di (PathHandler m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI

runPathHandler input (PH a) = do
  runStateT (runErrorT a) input
  return ()

----
end :: Monad m => PathHandler m ()
end = do
  r@Resource{resPath = []} <- PH get
  return ()

anySegment :: Monad m => PathHandler m T.Text
anySegment = PH $ do
  r@Resource{resPath = (x:xs)} <- get
  put r{resPath = xs}
  return x
               
segment :: Monad m => T.Text -> PathHandler m ()
segment s = do
  x <- anySegment
  guard $ x == s

lastSegment :: Monad m => PathHandler m T.Text
lastSegment = do
  s <- anySegment
  end
  return s

dirSegment :: Monad m => PathHandler m T.Text
dirSegment = do
  s <- anySegment
  r@Resource{resPath = (_:_)} <- PH get
  return s
    

----
clubviRoute :: 
  ( DepRecordMonad m SP DP
  , SiteConfig m
  , MonadSiteIO SP DP m) 
  => PathHandler m ()
clubviRoute = msum
  [ exactFile 
  , mainPage
  , return ()
  ]
  where
    exactFile = do
      d@Resource{..} <- PH get
      let s = Resource{..}
      doesExistSI s >>= guard
      lift $ do
        hs <- openSI s
        hd <- openDI d
        cts <- liftIO $  LBS.hGetContents hs
        liftIO $ LBS.hPutStr hd cts
      -- FIXME collect deps
    mainPage = do
      d@Resource{..} <- PH get
      let s = Resource{resName = resName `changeExt` "mp", .. }
      doesExistSI s >>= guard
      lift $ runMainPage 0 s d


changeExt :: T.Text -> T.Text -> T.Text
changeExt name ext = T.intercalate "." (baseNameChunks ++ [ext])
  where
    split = T.splitOn "." name
    baseNameChunks | x:y:xs <- split = init split
                   | otherwise = [name]
            


