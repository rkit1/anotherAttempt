{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ClubviRu.Date where
import Text.Parsec
import Library
import System.Time
import qualified Data.Map as M
import Data.Binary

data Date
  = NullDate
  | Date
    { dYear :: Int 
    , dMonth :: Month }
    deriving (Eq, Ord, Show)

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

readDate :: MonadPlus m => String -> m Date
readDate str = case parse (n `mplus` m) str str of
  Left err -> mzero
  Right res -> return res
  where
    n = do
      try $ string $ showDate NullDate
      return NullDate
    m = do
      dMonth <- msum $ flip map months
        $ \ (c, n) -> try (string n) >> return c
      char ' '
      dYear <- liftM read $ replicateM 4 digit
      string "г."
      return Date{..}

pt str m = parse m str str

showDate Date{..} = printf "%s %dг." (showMonth dMonth) dYear
showDate NullDate = "Дата неизвестна"

showMonth :: Month -> String
showMonth m = a
  where
    Just a = lookup m months

months :: [(Month, String)]
months =
  [ January   % "Январь" 
  , February  % "Февраль" 
  , March     % "Март" 
  , April     % "Апрель" 
  , May       % "Май" 
  , June      % "Июнь" 
  , July      % "Июль" 
  , August    % "Август" 
  , September % "Сентябрь" 
  , October   % "Октябрь" 
  , November  % "Ноябрь" 
  , December  % "Декабрь" ]
  where
    (%) = (,)

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

--fileNameFromDate :: FilePath -> Date -> FilePath
--fileNameFromDate prefix d = printf "%s/%s.htm" prefix (showDate d)



{-
test = do
  a <- readFile "x:/~listings/remember"
  forM_ (groupByMonths $ lines a) print
-}
