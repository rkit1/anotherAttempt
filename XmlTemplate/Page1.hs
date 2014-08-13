{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, NoMonomorphismRestriction, QuasiQuotes, OverloadedStrings, FlexibleContexts
  #-}
module XmlTemplate.Page where
import XmlTemplate.Monad
import XmlTemplate.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Combinator
import qualified Data.Text as T
import Text.XmlHtml
import Library hiding (many)
import qualified Data.Map as M
import qualified Data.Char as C
import Control.Monad.Writer

mkPage path title content = do
  HtmlDocument{..} <- readHtml "x:/templates/index.htm.src"
  let substs = [ "path" % [TextNode path]
               , "content" % content 
               , "title" % [TextNode title] ]
  (ws, docContent) <- runMT (M.fromList substs) (M.fromList bodyMacros) $ onNodes docContent
  mapM_ (liftIO . putStrLn) ws
  return HtmlDocument{..}


bodyMacros = [ "image" % image
             , "subst" % subst ]

image :: (Monad m) => Node -> MT m ()
image Element{..} = do
  let Just src = lookup "src" elementAttrs
      align' = lookup "align" elementAttrs
      align | Just a <- align', a `elem` ["left", "right"] = [a]
            | Just "center" <- align' = []
            | Nothing <- align' = []
            | Just a <- align' = error $ printf "image: wrong align: %s" $ show a
  e <- buildElement' "div" [ "class" % T.intercalate " " $ "img" : align ] $ do
    withSubsts [ "src" % [TextNode src] ] $ onNodes [hQ|<img src="$src$" alt="">|]
    parseT "<image> body" elementChildren $ do
      spaces
      msum
        [ (many1 anyToken >>= buildElement' "p" [] . yieldMany) >>= yield
        , eof ]
  yield e