{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable
  , EmptyDataDecls #-}
module ClubviRu.Resource where
import qualified Data.Text as T
import Data.String
import ClubviRu.Config.Site
import Control.Monad.Error
import Data.Data
import Control.Eff

----
type SourcePath = Resource Source
type DestinationPath = Resource Destination
type SP = Resource Source
type DP = Resource Destination


data Source
  deriving (Typeable)
data Destination
  deriving (Typeable)

class ResType t where
  resRoot :: HasSiteConfig r => Resource t -> Eff r FilePath

instance ResType Source where
  resRoot _ = sourceRoot

instance ResType Destination where
  resRoot _ = destinationRoot

data Resource a = Resource
  { resPathType :: ResPathType
  , resPath :: [T.Text]
  , resName :: T.Text
  } deriving (Show, Eq, Ord, Typeable, Data, Read)

data ResPathType = Relative | Absolute 
  deriving (Show, Eq, Ord, Typeable, Data, Read)

instance IsString (Resource a) where
  fromString str = Resource{..}
    where 
      chunks = T.splitOn "/" $ T.pack str
      resPathType | '/':xs <- str = Absolute
                  | otherwise = Relative
      resName | last chunks == "" || last chunks == "."
                = "index.htm"
              | otherwise = last chunks
      resPath = removeBeginningUps resPathType $ 
                  removeUps $ removeEmptySegments $ init chunks


----
toFilePathM :: (HasSiteConfig r, ResType t) => Resource t -> Eff r FilePath
toFilePathM res = do
  root <- resRoot res
  return $ toFilePath root res

toFilePath :: FilePath -> Resource a -> FilePath
toFilePath root Resource{..} =
    root ++ 
      '/' :
      concat [ T.unpack x ++ "/" | x <- removeBeginningUps Absolute resPath] ++ 
      T.unpack resName

toDirectoryPathM :: (HasSiteConfig r, ResType t) => Resource t -> Eff r FilePath
toDirectoryPathM res = do
  root <- resRoot res
  return $ toDirectoryPath root res

toDirectoryPath :: FilePath -> Resource a -> FilePath
toDirectoryPath root Resource{..} =
    root ++ 
      '/' :
      concat [ T.unpack x ++ "/" | x <- removeBeginningUps Absolute resPath]


pathToString :: Resource a -> FilePath
pathToString Resource{..} = '/' :
  concat [ T.unpack x ++ "/" | x <- removeBeginningUps Absolute resPath]


relativeTo :: Resource a -> Resource a -> Resource a
relativeTo a@Resource{resPathType = Absolute} b = a
relativeTo a@Resource{resPath = r} b@Resource{..} = 
    a{resPath = removeUps $ resPath ++ r, resPathType = resPathType}


----
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


----
getName :: Resource t -> T.Text
getName s@Resource{..} = getNameT resName


getNameT :: T.Text -> T.Text
getNameT name = n
  where
    split = T.splitOn "." name
    n | x:xs <- split = T.intercalate "." $ init split
      | otherwise = ""

    
getExt :: Resource t -> T.Text
getExt s@Resource{..} = getExtT resName

getExtT :: T.Text -> T.Text
getExtT name = ext
  where
    split = T.splitOn "." name
    ext | x:y:xs <- split = last split
        | otherwise = ""

  
changeExt :: Resource a -> T.Text -> Resource a
changeExt s@Resource{..} ext = s{resName = resName `changeExtT` ext}

changeExtT :: T.Text -> T.Text -> T.Text
changeExtT name ext = T.intercalate "." (baseNameChunks ++ [ext])
  where
    split = T.splitOn "." name
    baseNameChunks | x:y:xs <- split = init split
                   | otherwise = [name]
            

addExt :: Resource a -> T.Text -> Resource a
addExt s@Resource{..} ext = s{resName = resName `addExtT` ext}

addExtT :: T.Text -> T.Text -> T.Text
addExtT name ext = T.intercalate "." [name,ext]
