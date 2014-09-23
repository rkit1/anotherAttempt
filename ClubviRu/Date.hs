{-# LANGUAGE RecordWildCards, TemplateHaskell, FlexibleContexts, ScopedTypeVariables #-}
module ClubviRu.Date where
import Text.Parsec
import Library
import System.Time
import qualified Data.Map as M
import Data.Binary
import SiteGen.Main
import Data.Char
import Data.String
import ClubviRu.Resource
import qualified Data.Set as S

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




peekMonths :: (MonadSiteIO SP di t m, DepRecordMonad m SP di) =>
              SP -> String -> m [Date]
peekMonths prefix str = lastStep `liftM` foldM go S.empty ls
  where
    ls = selectLines str
    lastStep s = map getDate $ S.toList s
    getDate str = const NullDate <++> id $ parse dateParser "" str    
    go s l | Just fp <- "!list:" `stripPrefix` l = do
      let p = fromString fp `relativeTo` prefix
      str <- peek $ readString p          
      let lines = [ fromString x | x <- selectLines str ]
      return $! foldl' (flip S.insert) s lines
    go s l = return $! S.insert (fromString l) s

    
queryMonth :: (MonadSiteIO SP di t m, DepRecordMonad m SP di) =>
             SP -> String -> Date -> m [SP]
queryMonth prefix str d = concat `liftM` forM ls withLine
  where
    ls = selectLines str
    getDate str = const NullDate <++> id $ parse dateParser "" str
    withLine l | Just fp <- "!list:" `stripPrefix` l = do
      let p = fromString fp `relativeTo` prefix
      str <- peek $ readString p
      let ls = [ fromString x 
               | x <- selectLines str
               , getDate x == d ]
      when (not $ null ls) $ recordSI p
      return ls
    withLine l = return [ fromString l | getDate l == d ]

queryLines :: (MonadSiteIO SP di t m, DepRecordMonad m SP di) =>
              SP -> String -> Int -> m [SP]
queryLines prefix str count = go ls count
  where
    ls = selectLines str
    go [] _ = return []
    go _  0 = return []
    go (l:ls) count
      | Just p <- "!list:" `stripPrefix` l = do
          lstr <- readString $ fromString p `relativeTo` prefix
          let lls = selectLines lstr
              res = take count $ map fromString lls
          llss <- go ls (count - length lls)
          return $ res ++ llss
      | True = do
          llss <- go ls (count - 1)
          return $ fromString l : llss


selectLines :: String -> [String]
selectLines str = [ x | x <- lines str, not $ all isSpace x ]

dateParser = parser
  where
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


groupByMonths :: [FilePath] -> [(Date,[FilePath])]
groupByMonths ps = M.toDescList m
  where
    m = M.fromListWith (++) $ map (getDate &&& (:[])) ps
    getDate str = const NullDate <++> id $ parse dateParser "" str

--fileNameFromDate :: FilePath -> Date -> FilePath
--fileNameFromDate prefix d = printf "%s/%s.htm" prefix (showDate d)



{-
test = do
  a <- readFile "x:/~listings/remember"
  forM_ (groupByMonths $ lines a) print
-}
