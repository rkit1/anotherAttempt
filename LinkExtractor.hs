{-# LANGUAGE ImplicitParams #-}
module LinkExtractor where
import Path.Destination
import Path
import Data.List
import Text.HTML.TagSoup
import Control.Effects.Error
import Control.Effects.Writer
import Control.Effects
import Control.Monad

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

-- FIXME filter absolute 
extractLinks ::
  ( ?warnings :: Effect ([String], r) m1
  , AutoLift ([String], r) m1 m
  ) =>
    String -> m [DestinationPath]
extractLinks str = liftM concat $ forM (extractLinkStrings str) $ \ a -> case a of
    Left warn -> tell ?warnings ["HTML PARSE: " ++ warn] >> return []
    Right line 
      | 
      | otherwis -> case toDestinationPath line of
        Left err -> tell ?warnings ["LINK PARSE: " ++ err] >> return []
        Right res -> return [res]
