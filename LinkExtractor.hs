{-# LANGUAGE ImplicitParams, CPP #-}
module LinkExtractor where
import Path.Destination
import Path
import Data.List
import Text.HTML.TagSoup
import Control.Effects.Error
import Control.Effects.Writer
import Control.Effects
import Control.Monad
import Network.URL

extractLinkStrings :: String -> [Either String String]
extractLinkStrings str = parseTags str >>= f
  where
    f (TagOpen t attrs) | Just (_, attr)   <- find (\(t', _) -> t' == t) table
                        , Just (_, val)    <- find (\(a', _) -> a' == attr) attrs
                        = [Right val]
    f (TagWarning w) = [Left w]
    f _ = []
    table =
      [ "a" % "href" 
      , "img" % "src" 
      , "link" % "href"
      , "script" % "src" ]
    (%) = (,)


extractLinks ::
  ( ?warnings :: Effect ([String], r) m1
  , AutoLift ([String], r) m1 m
  ) =>
    String -> m [URL]
extractLinks str = liftM concat $ forM (extractLinkStrings str) $ \ a -> case a of
    Left warn -> tell ?warnings ["HTML PARSE: " ++ warn] >> return []
    Right line -> case importURL  line of
      Nothing -> tell ?warnings ["importURL ERROR: \"" ++ line ++ "\""] >> return []
      Just url -> return [url]


#ifdef dev
test = do
  x <- readFile "x:/index.htm"
  print $ run $ with writer $ \ w -> 
      let ?warnings = w in extractLinks x
    
  
#endif

