{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ClubviRu.Resource where
import qualified Data.Text as T
import Data.String

data Source
data Destination

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
      resPath = removeBeginningUps resPathType $ 
                  removeUps $ removeEmptySegments $ init chunks


toFilePath :: FilePath -> Resource a -> FilePath
toFilePath root Resource{..} =
    root ++ '/' :
      concat [ T.unpack x ++ "/" | x <- removeBeginningUps Absolute resPath] ++ 
      T.unpack resName

data ResPathType = Relative | Absolute deriving Show

relativeTo :: Resource a -> Resource a -> Resource a
relativeTo a@Resource{resPathType = Absolute} b = a
relativeTo a@Resource{resPath = r} b@Resource{..} = 
    a{resPath = removeUps $ resPath ++ r, resPathType = resPathType}


removeEmptySegments :: [T.Text] -> [T.Text]
removeEmptySegments = filter (\ x -> x /= "" && x /= ".") 

removeBeginningUps :: ResPathType -> [T.Text] -> [T.Text]
removeBeginningUps Relative x = x
removeBeginningUps _ ("..":xs) = removeBeginningUps Absolute xs
removeBeginningUps _ a@(x:xs) = a
removeBeginningUps _ [] = []

removeUps :: [T.Text] -> [T.Text]
removeUps a@("..":xs) = a
removeUps (x:"..":xs) = removeUps xs
removeUps (x:xs) = x:removeUps xs
removeUps [] = []

{-

test = mapM_ print x
  where 
    x :: [Resource a]
    x = map fromString 
        [ "./asd/ph.htm"
        , "ph.htm"
        , "/pg.htm"
        , "../asd/dsa"
        , "/../p"]


-}

