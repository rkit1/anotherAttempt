{-# LANGUAGE ImplicitParams, CPP #-}
module SiteGen.LinkExtractor where
import Data.List
import Text.HTML.TagSoup

extractLinkStrings :: String -> [String]
extractLinkStrings str = parseTags str >>= f
  where
    f (TagOpen t attrs) | Just (_, attr)   <- find (\(t', _) -> t' == t) table
                        , Just (_, val)    <- find (\(a', _) -> a' == attr) attrs
                        = [val]
    f _ = []
    table =
      [ "a" % "href" 
      , "img" % "src" 
      , "link" % "href"
      , "script" % "src" ]
    (%) = (,)
