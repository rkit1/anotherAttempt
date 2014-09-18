{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, NoMonomorphismRestriction, QuasiQuotes, OverloadedStrings
  #-}
module ClubviRu.Pages.Mainpage.WidgetHeadImg where
import XmlTemplate
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import Text.XmlHtml
import Library
import qualified Data.Map as M

wImgTag = ("r:img", f)
  where
    f Element{..} = do
      case P.feed (foldl P.feed (P.parse parser "") $ textFromNodes elementChildren) "" of
        a@P.Fail{..} -> error $ "rememberImgTag: " ++ show a
        P.Done _ r -> mkImg r
    parser = many1 decimal
    mkImg [i] = withSubst ("d", [TextNode $ T.pack $ show i]) $ do
                mapM_ onNode [hQ|<a href="$path$/$d$.jpg"><img src="$path$/$d$z.jpg"/></a> |]

x = runM (M.fromList [("path", [hQ|/tmp/|])]) (M.fromList [wImgTag]) $ mapM_ onNode [hQ|<r:img>1</r:img>|]

