{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving,
  RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
  NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables  #-}
module ClubviRu.Route where
import ClubviRu.Config.Site
import ClubviRu.Resource
import Library
import ClubviRu.Pages.Mainpage
import ClubviRu.DepFile
import ClubviRu.Date
import System.Directory
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error
import SiteGen.Main
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Text.Printf
import Data.String
import ClubviRu.URIParser
import SiteGen.LinkExtractor
import Control.Applicative
import Data.Time
import Control.Exception.Lifted
import Control.Monad.Trans.Control 

----
newtype PathHandler m a =
  PH { runPH :: ErrorT String (StateT (Resource Destination) m) a }
    deriving (Functor, Applicative, Alternative, Monad
             , MonadPlus, MonadIO, MonadError String)

runPathHandler
  :: forall m a. MonadBaseControl IO m
  => DP -> PathHandler m a -> m (Either String a)
runPathHandler input (PH a) = catch work handler
  where
    work = evalStateT (runErrorT a) input
    handler :: SomeException -> m (Either String a)
    handler e = return $ Left $ "runPathHandler: " ++ show input ++ ": " ++ show e


instance Monad m => MonadState DP (PathHandler m) where
  get = PH get
  put s = PH $ put s
  
instance MonadTrans PathHandler where
  lift m = PH $ lift $ lift m

instance MonadSiteIO si di t m => MonadSiteIO si di t (PathHandler m) where
  openSI = lift . openSI
  openDI = lift . openDI
  doesExistSI = lift . doesExistSI
  checkTime = lift . checkTime
  curTime = lift curTime

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
  , MonadSiteIO SP DP UTCTime m
  , MonadPlus m
  , MonadState DP m
  , MonadError String m)
  => PathHandlerM m

instance
  ( DepRecordMonad m SP DP
  , SiteConfig m
  , MonadSiteIO SP DP UTCTime m )
  => PathHandlerM (PathHandler m)


----
clubviRoute :: (PathHandlerM m) => m ()
clubviRoute = msum
  [ exactFile
  , archive
  , mainPage
  , mainPageListing
  , apps
  ] `mplus` unhandled
  where
    unhandled = do
      d <- get
      throwError $ printf "unhandled desination: %s" (show d)

apps :: (PathHandlerM m) => m ()
apps = do
  Resource{resPath = "apps":_} <- get
  return ()

mainPage :: (PathHandlerM m) => m ()
mainPage = do
  d@Resource{..} <- get
  let s = Resource{resName = resName `changeExtT` "mp", .. }
  doesExistSI s >>= guard
  str <- runMainPage Nothing s
  recordHtmlLinks str d
  writeString d str
  depFile


mainPageListing :: (PathHandlerM m) => m ()
mainPageListing = do
  d@Resource{..} <- get
  let s = Resource{resName = resName `changeExtT` "mpl", .. }
  doesExistSI s >>= guard
  str <- runMainPageListing s
  recordHtmlLinks str d
  writeString d str
  depFile


archive :: PathHandlerM m => m ()
archive = do
  d@Resource{resPath = ("archive":path), ..} <- get
  let s = Resource{resName = last path `changeExtT` "mp", resPath = init path, ..}
  doesExistSI s >>= guard
  date <- readDate $ T.unpack resName
  str <- runMainPage (Just date) s
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
  depFile
  
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
