{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module PlainTemplate.Listing where
import Text.Parsec
import Library
import System.Time
import qualified Data.Map as M
import Data.Binary
import qualified Store as S

data Date 
  = Date
    { dYear :: Int 
    , dMonth :: Month }
  | NullDate
    deriving (Eq, Ord)

instance Binary Date where
  put Date{..} = do
    putWord8 1
    put dYear
    put dMonth
  put NullDate = putWord8 2
  get = do
    x <- getWord8
    case x of
      1 -> do
        dYear <- get
        dMonth <- get
        return Date{..}
      2 -> return NullDate

instance Binary Month where
  put m = putWord8 $ fromIntegral $ fromEnum m
  get = (toEnum . fromIntegral) `liftM` getWord8

showDate Date{..} = printf "%s %dг." (showMonth dMonth) dYear
showDate NullDate = "Дата неизвестна"
                    
showMonth January   = "Январь"
showMonth February  = "Февраль"
showMonth March     = "Март"
showMonth April     = "Апрель"
showMonth May       = "Май"
showMonth June      = "Июнь"
showMonth July      = "Июль"
showMonth August    = "Август"
showMonth September = "Сентябрь"
showMonth October   = "Октябрь"
showMonth November  = "Ноябрь"
showMonth December  = "Декабрь"

groupByMonths :: [FilePath] -> [(Date,[FilePath])]
groupByMonths ps = M.toDescList m
  where
    m = M.fromListWith (++) $ map (getDate &&& (:[])) ps
    getDate str = const NullDate <++> id $ parse parser "" str
    parser = do
      string "/news/"
      y <- year
      char '/'
      m <- month
      return $ Date y m
    year = read `liftM` replicateM 4 digit
    month = do
      a <- replicateM 2 digit
      return $ toEnum $ read a - 1

readListing :: MonadIO m => FilePath -> m [String]
readListing listing = liftIO $ lines `liftM` readFile listing 

fileNameFromDate :: FilePath -> Date -> FilePath
fileNameFromDate prefix d = printf "%s/%s.htm" prefix (showDate d)




{-test = do
  a <- readFile "x:/~listings/index"
  print $ groupByMonths $ lines a-}