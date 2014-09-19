{-# LANGUAGE OverloadedStrings #-}
module Main where
import ClubviRu.Storage
import ClubviRu.Resource
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Exception

main :: IO b
main = do
  s <- returnStorage
  forever $ t $ do
    str <- getLine
    let dp = read str
    forM_ (M.toList s)
      $ \ (sdi, res) -> case res of
        Left err -> return ()
        Right (t, ss, dd) -> forM_ (S.toList dd)
          $ \ di -> when (di == dp) $ putStrLn $ toFilePath "" sdi

t :: IO a -> IO ()
t a = do
  r <- try a
  case r of Left (SomeException e) -> print e
            _ -> return ()
