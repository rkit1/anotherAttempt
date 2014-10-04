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
import Control.Eff
import Control.Eff.State.Strict
import Control.Eff.Exception
import Control.Eff.Lift

{-
----

runPathHandler
  :: forall m a. MonadBaseControl IO m
  => DP -> PathHandler m a -> m (Either String a)
runPathHandler input (PH a) = catch work handler
  where
    work = evalStateT (runErrorT a) input
    handler :: SomeException -> m (Either String a)
    handler e = return $ Left $ "runPathHandler: " ++ toFilePath "" input ++ ": " ++ show e

-}

class ( Member (State DP) r, MonadPlus (Eff r), Member (Exc String) r
      , HasSiteIO SP DP UTCTime r, HasDepRecord SP DP r, HasSiteConfig r)
      => HasPathHandler r
--instance (Member (State DP) r, MonadPlus (Eff r), Member (Exc String) r)
--         => HasPathHandler r


----
clubviRoute :: (HasPathHandler r, Typeable m, SetMember Lift (Lift m) r, MonadIO m) => Eff r ()
clubviRoute = msum
  [ exactFile
  , archive
  , mainPage
  , mainPageListing
  , apps
  ] `mplus` do
    d :: DP <- get
    throwExc $
      (printf "unhandled desination: %s" (toFilePath "" d) :: String)

apps :: HasPathHandler r => Eff r ()
apps = do
  Resource{resPath = "apps":_} :: DP <- get
  return ()

mainPage :: (HasPathHandler r, Typeable m, SetMember Lift (Lift m) r, MonadIO m) => Eff r ()
mainPage = do
  d@Resource{..} <- get
  let s = Resource{resName = resName `changeExtT` "mp", .. }
  doesExistSI s >>= guard
  str <- runMainPage Nothing s
  recordHtmlLinks str d
  writeString d str
  depFile


mainPageListing :: (HasPathHandler r, Typeable m, SetMember Lift (Lift m) r, MonadIO m) => Eff r ()
mainPageListing = do
  d@Resource{..} <- get
  let s = Resource{resName = resName `changeExtT` "mpl", .. }
  doesExistSI s >>= guard
  str <- runMainPageListing s
  recordHtmlLinks str d
  writeString d str
  depFile


archive :: (HasPathHandler r, Typeable m, SetMember Lift (Lift m) r, MonadIO m) => Eff r ()
archive = do
  d@Resource{resPath = ("archive":path), ..} <- get
  let s = Resource{resName = last path `changeExtT` "mp", resPath = init path, ..}
  doesExistSI s >>= guard
  date <- readDate $ T.unpack resName
  str <- runMainPage (Just date) s
  recordHtmlLinks str d
  writeString d str

exactFile :: HasPathHandler r => Eff r ()
exactFile = do
  d@Resource{..} <- get
  let s = Resource{..}
  doesExistSI s >>= guard
  msum
    [ copyHtmlAndRecord s d
    , copySItoDI s d ]
  depFile
  
depFile :: HasPathHandler r => Eff r ()
depFile = do
  d@Resource{..} <- get
  let s = Resource{..}
  let df = s `addExt` "deps"
  ( do
    doesExistSI df >>= guard
    deps <- readDepFile df
    forM_ deps $ \ dep -> recordDI $ dep `relativeTo` d
    ) `mplus` return ()




copyHtmlAndRecord :: HasPathHandler r => SP -> DP -> Eff r ()
copyHtmlAndRecord s d = do
  guard $ getExt d == "htm"
  str <- readString s
  writeString d str
  recordHtmlLinks str d
    


recordHtmlLinks :: HasPathHandler r => String -> DP -> Eff r ()
recordHtmlLinks str dp = do
  links <- filterLinks $ extractLinkStrings str
  forM_ links $ \ l -> recordDI $ fromString l `relativeTo` dp
