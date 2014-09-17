{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module ClubviRu.Config.Parser where
import qualified Data.Map as M
import Control.Monad.Trans
import Text.Peggy
import SiteGen.IO as IO
import SiteGen.Main
import Control.Monad


parseConfigFile :: MonadIO m => FilePath -> m (Either ParseError (M.Map String String))
parseConfigFile fp = liftIO $ do
  a <- parseFile configParser fp
  return $ fmap linesToMap a

readConfig :: (DepRecordMonad m si di, MonadSiteIO si di t m)
  => si -> m (Either ParseError (M.Map String String))
readConfig res = parseConfig `liftM` IO.readString res

parseConfig :: String -> Either ParseError (M.Map String String)
parseConfig str = fmap linesToMap $ parseString configParser "config string" str

linesToMap :: [Line] -> M.Map String String
linesToMap ls = go M.empty ls
  where
    go map [] = map
    go map (HeaderLine h:ls) = go (M.insert h str map) tail
      where (str, tail) = collectRegulars ("", ls)
    collectRegulars (str, RegularLine rl:ls) = collectRegulars (str ++ rl, ls)
    collectRegulars x = x


data Line
  = HeaderLine String
  | RegularLine String
    deriving Show

[peggy|

configParser :: [Line]
  = '-- ' [a-zA-Z0-9]+ '\n' configParser { HeaderLine $1 : $2 }
  / [^\n]+ '\n' configParser { RegularLine ($1 ++ "\n") : $2 }
  / "--" !. { [] }

|]

--test :: IO (Either ParseError (M.Map String String))
--test = parseConfigFile "c:\\Users\\Victor\\Documents\\wrk\\newsite\\anotherAttempt\\Config\\mainpageConfig"
