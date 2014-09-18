{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, NoMonomorphismRestriction, QuasiQuotes, OverloadedStrings
  #-}
module ClubviRu.Pages.Mainpage.Imgs where
import XmlTemplate
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import Text.XmlHtml
import Library
import qualified Data.Map as M


imgsTag = ("imgs", f)
  where
    f Element{..} = do
      case P.feed (foldl P.feed (P.parse parser "") $ textFromNodes elementChildren) "" of
        a@P.Fail{..} -> error $ show a
        P.Done _ r -> mapM_ mkImg r
    parser = do
      skipSpace
      a <- many1 $ do
        n <- decimal
        skipSpace >> return (n ::Int)
      endOfInput 
      return a
    mkImg i = withSubst ("d", [TextNode $ T.pack $ show i]) $ do
                mapM_ onNode [hQ|<a href="$path$/$d$.jpg"><img src="$path$/$d$s.jpg"/></a> |]

x = runM (M.fromList [("path", [hQ|/tmp/|])]) (M.fromList [imgsTag]) $ mapM_ onNode [hQ|<imgs>1 2 3</imgs>|]

