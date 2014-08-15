{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving,
  RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
  NoMonomorphismRestriction, FlexibleContexts #-}
module ClubviRu.Route where
import ClubviRu.Path
import ClubviRu.Debug.Helpers
import System.Directory
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error
import SiteGen.IO
import qualified Data.Text as T


mapToFile :: DestinationPath -> DestinationPath
mapToFile (DP a) = DP $ g a
  where
    g ("":[]) = ["index.htm"]
    g (x:xs) = x:g xs


----
newtype PathHandler m a = PH { runPH :: ErrorT String (StateT PathHandlerState m) a }
  deriving (Monad, MonadPlus, MonadIO)

instance MonadTrans PathHandler where
  lift m = PH $ lift $ lift m

instance MonadSiteIO si di m => MonadSiteIO si di (PathHandler m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  copySI si di = lift $ copySI si di 

data PathHandlerState = PHS
  { remaining :: [T.Text]
  , complete :: [T.Text] }


runPathHandler input (PH a) = runStateT (runErrorT a) input


----
end :: Monad m => PathHandler m ()
end = do
  PHS{remaining = []} <- PH get
  return ()

anySegment :: Monad m => PathHandler m T.Text
anySegment = PH $ do
  p@PHS{remaining = (x:xs), ..} <- get
  put p{remaining = xs, complete = x:complete}
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
  PHS{remaining = (_:_)} <- PH get
  return s
    

clubviRoute 
  :: ( Monad m
     , MonadSiteIO SourcePath DestinationPath m)
  => PathHandler m ()
clubviRoute = msum
  [ physicalFile 
  ]
  where
    physicalFile = msum
      [ end >> fileExists >> copyFile
      , anySegment >> physicalFile ]
    fileExists = do
      PHS{..} <- PH get
      c <- doesExistSI $ SP complete
      guard c
    copyFile = do
      PHS{..} <- PH get      
      copySI (SP complete) (DP complete)
      -- FIXME Check deps
