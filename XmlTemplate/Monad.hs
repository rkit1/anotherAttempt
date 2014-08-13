{-# LANGUAGE TemplateHaskell, FlexibleInstances, RecordWildCards, OverloadedStrings, TupleSections #-}
module XmlTemplate.Monad where
import Text.XmlHtml
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Trans.State (StateT(..))
import Data.Accessor.Template
import Data.Accessor.Monad.MTL.State hiding (lift)
import Data.Attoparsec.Text as P
import qualified Text.Parsec as PS
import Library
import Control.Monad.Identity
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH (lift,Lift)
import qualified Data.ByteString.UTF8 as U8
import qualified Blaze.ByteString.Builder as BZ
import qualified Data.ByteString as BS
import Control.Monad.Writer

class Monad m => ProcessM m where
  yield :: Node -> m ()
  yieldMany :: [Node] -> m ()
  yieldMany = mapM_ yield
  onNode :: Node -> m ()
  onNodes :: [Node] -> m ()
  onNodes = mapM_ onNode
  collect :: m a -> m (a,[Node])
  warning :: String -> m ()

instance (ProcessM m) => ProcessM (PS.ParsecT s u m) where
  yield = lift . yield
  yieldMany = lift . yieldMany
  onNode = lift . onNode
  onNodes = lift . onNodes
  collect m = PS.mkPT $ \ s -> do
    (a,b) <- collect $ PS.runParsecT m s
    return $ fmap (liftM (fmap (,b))) a
  warning = lift . warning

instance (Monoid w, ProcessM m) => ProcessM (WriterT w m) where
  yield = lift . yield
  yieldMany = lift . yieldMany
  onNode = lift . onNode
  onNodes = lift . onNodes
  collect m = WriterT $ do
    ((a, w),x) <- collect $ runWriterT m
    return ((a,x),w)
  warning = lift . warning


type MT m = StateT (MS m) m
type M = MT Identity

data MS m = MS
  { buildingNow_ :: [Node] -> [Node]
  , substs_ :: M.Map T.Text [Node]
  , macros_ :: M.Map T.Text (Node -> MT m ())
  , warnings_ :: [String] -> [String] }



runMT :: Monad m => M.Map T.Text [Node] -> M.Map T.Text (Node -> MT m ()) -> MT m a -> m ([String], [Node])
runMT substs_ macros_ m = do  
  let buildingNow_ = id
      warnings_ = id
  (_, MS{..}) <- runStateT m MS{..}
  return $ (warnings_ [], buildingNow_ [])

runM :: M.Map T.Text [Node] -> M.Map T.Text (Node -> M ()) -> M a -> ([String], [Node])
runM a b c = runIdentity $ runMT a b c

$(deriveAccessors ''MS)


instance Monad m => ProcessM (StateT (MS m) m) where
  yield n = modify buildingNow (.(n:))
  yieldMany ns = modify buildingNow (.(ns++))
  collect m = do
    c <- get buildingNow
    set buildingNow id
    a <- m
    c' <- get buildingNow
    set buildingNow c
    return $ (a, c' [])
  warning w = modify warnings (.(w:))
  onNode a@TextNode{} = yield a
  onNode c@Comment{} = return ()
  onNode e@Element{..} = do
    ms <- get macros
    case M.lookup elementTag ms of
      Just a -> a e
      Nothing -> do
        elementAttrs <- processAttrs e
        a <- buildElement' elementTag elementAttrs $ mapM_ onNode elementChildren
        yield a
    

buildElement elementTag elementAttrs m = do
  (a, elementChildren) <- collect m 
  return (a, Element{..})

buildElement' a b c = snd `liftM` buildElement a b c

processAttrs Element{..} = mapM replaceSubst elementAttrs
  where
    replaceSubst (n, v) = do
      let res = parseSubst v
      a <- mapM go res
      return (n, T.concat a)
    go (Left a) = return a
    go (Right b) = do
      ss <- get substs
      case M.lookup b ss of
        Just a -> return $ T.concat $ textFromNodes a
        Nothing -> error $ printf "subst not found: %s" $ T.unpack b

parseSubst t = case P.eitherResult $ P.feed (P.parse parser t) "" of
        Left err -> error err
        Right res -> res
  where
    parser = ( P.many1 $ msum
           [ liftM Left $ P.takeWhile1 $ \ a -> a /= '\\' && a /= '$'
           , liftM Left $ P.string "\\$"
           , liftM Right $ tag
           , liftM Left $ P.take 1 ] ) `mplus` return [Left ""]
    tag = do
      P.try $ P.char '$'
      a <- P.takeWhile1 $ \ a -> a /= '$'
      P.char '$'
      return a


withSubst ns m = do
  s <- get substs
  set substs (uncurry M.insert ns s)
  m
  set substs s

withSubsts ns m = do
  s <- get substs
  set substs (foldl (flip $ uncurry M.insert) s ns)
  m
  set substs s

textFromNodes l = concatMap g l
  where
    g (TextNode a) = [a]
    g Comment{} = []
    g Element{} = error "element passed to textFromNodes"

instance TH.Lift Node where
  lift (TextNode a) = [|TextNode $(TH.lift a)|]
  lift (Comment a) = [|Comment $(TH.lift a)|]
  lift (Element a b c) = [|Element $(TH.lift a) $(TH.lift b) $(TH.lift c)|]

instance TH.Lift T.Text where
  lift t = [| T.pack $(TH.lift $ T.unpack t) |]

hQ = QuasiQuoter
  { quoteExp = \ str -> case parseXML "template" $ U8.fromString str of
       Right XmlDocument{..} -> TH.lift docContent
       Left x -> error x
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined }

printNodes docContent = BZ.toByteStringIO BS.putStr $ nodesToBLDR docContent

nodesToBLDR docContent = render HtmlDocument{..}
    where docEncoding = UTF8
          docType = Nothing

readHtml :: MonadIO m => FilePath -> m Document
readHtml path = do
  s <- liftIO $ BS.readFile ("x:/" ++ path ++ "/~head.htm.src")
  case parseHTML path s of
    Right d -> return d
    Left err -> error $ printf "error while trying to parse %s: %s" path err

testMT s m m' = do
  (ws, ns) <- runMT s m m'
  mapM_ putStrLn ws
  printNodes ns


subst :: ProcessM m => Node -> MT m ()
subst Element{..} = do
  ss <- get substs
  let Just nm = lookup "name" elementAttrs
      Just a = M.lookup nm ss
  onNodes a
                    
