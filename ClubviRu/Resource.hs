{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ClubviRu.Resource where
import qualified Data.Text as T
import Data.String

data Resource a = Resource
  { resPathType :: ResPathType
  , resPath :: [T.Text]
  , resName :: T.Text
  } deriving Show

instance IsString (Resource a) where
  fromString str = Resource{..}
    where 
      chunks = T.splitOn "/" $ T.pack str
      resPathType | '/':xs <- str = Absolute
                  | otherwise = Relative
      resName | last chunks == "" 
              || last chunks == "." = "index.htm"
              | otherwise = last chunks
      resPath = removeUps $ removeEmptySegments $ init chunks


data ResPathType = Relative | Absolute deriving Show

relativeTo :: Resource a -> Resource a -> Resource a
relativeTo a@Resource{resPathType = Absolute} b = a
relativeTo a@Resource{resPath = r} b@Resource{..} = 
    a{resPath = removeUps $ resPath ++ r, resPathType = resPathType}


removeEmptySegments :: [T.Text] -> [T.Text]
removeEmptySegments = filter (\ x -> x == "" || x == ".") 

removeUps :: [T.Text] -> [T.Text]
removeUps a@("..":xs) = a
removeUps (x:"..":xs) = removeUps xs
removeUps (x:xs) = removeUps xs
removeUps [] = []

{-

*ClubviRu.Resource> fromString "./asd/ph.htm" :: Resource a
Resource {resPathType = Relative, resPath = [], resName = "ph.htm"}


pg.htm
/pg.htm
./asd/dsa

-}