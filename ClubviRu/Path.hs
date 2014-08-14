{-# LANGUAGE OverloadedStrings, CPP#-}
module ClubviRu.Path where
import qualified Data.Text as T
import Data.String
import SiteGen.IO
import SiteGen.Deps
import ClubviRu.Config.Site
#ifdef dev
import Control.Monad
#endif


-- global = "":xs
-- relative = x:xs



------
class Path p where
    relativeTo :: p -> p -> p
    toFilePath :: SiteConfig m => p -> m FilePath



------
newtype DestinationPath = DP [T.Text] 
    deriving (Show, Ord, Eq)

instance Path DestinationPath where
    DP a `relativeTo` DP b = DP $ normalizePath $ a `pathRelativeTo` b
    toFilePath (DP a) = do
      fp <- destinationRoot
      return $ toFilePath' fp a

instance IsString DestinationPath where
    fromString str = DP $ readPath str
    

------
newtype SourcePath = SP [T.Text]
    deriving (Show, Ord, Eq)

instance Path SourcePath where
    SP a `relativeTo` SP b = SP $ normalizePath $ b ++ a
    toFilePath (SP a) = do
      fp <- sourceRoot
      return $ toFilePath' fp a

instance IsString SourcePath where
    fromString str = SP $ readPath str



------
readPath :: String -> [T.Text]
readPath "" = [".", ""]
readPath "." = [".", ""]
readPath ".." = ["..", ""]
readPath str = normalizePath $ T.splitOn "/" $ T.pack str

normalizePath :: [T.Text] -> [T.Text]
normalizePath ls = case ls of
  ".":"..":xs  -> ".":"..":normalizePath xs
  "..":"..":xs -> "..":"..":normalizePath xs
  x:"..":xs    -> normalizePath xs
  x:xs         -> x:normalizePath xs
  []           -> []


pathRelativeTo :: [T.Text] -> [T.Text] -> [T.Text]
pathRelativeTo a@("":_) _ = a
pathRelativeTo (".":a) b = init b ++ a
pathRelativeTo a b = init b ++ a

toFilePath' :: FilePath -> [T.Text] -> FilePath
toFilePath' sfp xs = sfp ++ concat [ '/':T.unpack x | x <- clear xs ]
  where
    xs' = clear xs
    clear (".":xs) = clear xs
    clear ("..":xs) = clear xs
    clear ("":xs) = clear xs
    clear xs = xs


#ifdef dev


test = forM_ [ "."
             , ""
             , "." `relativeTo` "/home/index.htm"
             , "." `relativeTo` "/home/index.htm/"
             , ".." `relativeTo` "/home/index.htm/"
             , ".." `relativeTo` "/home/index.htm"
             , ".." `relativeTo` ".." 
             , "/"
             , "/home"
             , "home/"
             ] $ \ x@(DP y) -> do
          print x
          putStrLn $ toFilePath' "x:" y



#endif


