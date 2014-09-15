{-# LANGUAGE OverloadedStrings #-}
module ClubviRu.Debug.FindDepSource where
import ClubviRu.Storage
import ClubviRu.Resource
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

findDPSource :: DP -> IO ()
findDPSource dp = do
  s <- returnStorage
  forM_ (M.toList s)
    $ \ (sdi, res) -> case res of
      Left err -> return ()
      Right (t, ss, dd) -> forM_ (S.toList dd)
        $ \ di -> when (di == dp) $ print sdi

find :: IO ()
find = findDPSource myRes

myRes = Resource {resPathType = Absolute, resPath = ["archive","index"], resName = "index.htm"}
