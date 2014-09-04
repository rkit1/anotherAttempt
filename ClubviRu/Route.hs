{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving,
  RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
  NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables  #-}
module ClubviRu.Route where
import ClubviRu.Config.Site
import ClubviRu.Resource
import ClubviRu.Debug.Helpers
import ClubviRu.Pages.Mainpage
import ClubviRu.DepFile
import System.Directory
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error
import SiteGen.IO
import SiteGen.Deps
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Text.Printf
import Data.String
import ClubviRu.URIParser
import SiteGen.LinkExtractor


----
newtype PathHandler m a =
  PH { runPH :: ErrorT String (StateT (Resource Destination) m) a }
    deriving (Monad, MonadPlus, MonadIO)

runPathHandler input (PH a) = do
  runStateT (runErrorT a) input
  return ()

instance Monad m => MonadState DP (PathHandler m) where
  get = PH get
  put s = PH $ put s
  
instance MonadTrans PathHandler where
  lift m = PH $ lift $ lift m

instance MonadSiteIO si di m => MonadSiteIO si di (PathHandler m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI

instance SiteConfig m => SiteConfig (PathHandler m) where
  sourceRoot = lift sourceRoot
  destinationRoot = lift destinationRoot
  storeRoot = lift storeRoot
  myDomains = lift myDomains
  
instance DepRecordMonad m SP DP => DepRecordMonad (PathHandler m) SP DP where
  recordSI = lift . recordSI
  recordDI = lift . recordDI

class
  ( DepRecordMonad m SP DP
  , SiteConfig m
  , MonadSiteIO SP DP m
  , MonadPlus m
  , MonadState DP m) 
  => PathHandlerM m

instance
  ( DepRecordMonad m SP DP
  , SiteConfig m
  , MonadSiteIO SP DP m ) 
  => PathHandlerM (PathHandler m)


----
clubviRoute :: (PathHandlerM m) => m ()
clubviRoute = msum
  [ exactFile >> depFile
  , mainPage >> depFile
  , unhandled ] 
  where
    unhandled = do
      d <- get
      liftIO $ printf "unhandled desination: %s\n" (show d)

mainPage :: (PathHandlerM m) => m ()
mainPage = do
  d@Resource{..} <- get
  let s = Resource{resName = resName `changeExtT` "mp", .. }
  doesExistSI s >>= guard
  str <- runMainPage 0 s
  recordHtmlLinks str d
  writeString d str

exactFile :: (PathHandlerM m) => m ()
exactFile = do
  d@Resource{..} <- get
  let s = Resource{..}
  doesExistSI s >>= guard
  msum
    [ copyHtmlAndRecord s d
    , copyAnything s d ]

depFile :: (PathHandlerM m) => m ()
depFile = do
  d@Resource{..} <- get
  let s = Resource{..}
  let df = s `addExt` "deps"
  ( do
    doesExistSI df >>= guard
    deps <- readDepFile df
    forM_ deps $ \ dep -> recordDI $ dep `relativeTo` d
    ) `mplus` return ()

----
copyHtmlAndRecord :: (PathHandlerM m) => SP -> DP -> m ()
copyHtmlAndRecord s d = do
  guard $ getExt d == "htm"
  str <- readString s
  writeString d str
  recordHtmlLinks str d
    

copyAnything :: (PathHandlerM m) => SP -> DP -> m ()
copyAnything s d = do
  str <- readByteStringL s
  writeByteStringL d str


----
recordHtmlLinks :: (PathHandlerM m) => String -> DP -> m ()
recordHtmlLinks str dp = do
  links <- filterLinks $ extractLinkStrings str
  forM_ links $ \ l -> recordDI $ fromString l `relativeTo` dp
