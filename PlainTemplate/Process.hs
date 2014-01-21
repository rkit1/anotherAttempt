{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module PlainTemplate.Process where

import PlainTemplate.Variable
import PlainTemplate.Parser
import PlainTemplate.Monad
import PlainTemplate.NeedRebuild
import Head
import Library
import Library.System
import Data.Accessor.Monad.MTL.State ((%=),(%:))
import qualified Data.Accessor.Monad.MTL.State as A
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Set as S
import System.Process
import System.FilePath
import qualified Text.Parsec as P
import Cache

callRTPL :: FilePath -> M String
callRTPL path = do
  recordDepend path
  readRTPLE utf8 path >>= processTemplate

readRTPL :: [Char] -> M Body
readRTPL path = do
  f <- liftIO $ readFile $ "x:" ++ path
  throwError . show <++> return 
    $ runP body () path f

readRTPLE :: TextEncoding -> [Char] -> M Body
readRTPLE enc path = do
  f <- liftIO $ readFileE enc $ "x:" ++ path
  throwError . show <++> return 
    $ runP body () path f

parseRTPL :: (MonadError String m) => String -> m Body
parseRTPL str = throwError . show <++> return  $ runP body () "no file" str

processTemplate :: Body -> M String
processTemplate = wide $ return <++> p
  where 
    p = setVarTag ( \ a b -> setVar a b >> return [] )
      $ stepDown
      $ variableTag mcast
      $ imagesTag return
      $ includeTag processTemplate
      $ foreachTag (\ body v -> updateVars v >> processTemplate body )
      $ unexpectedTag

wide m l = liftM concat $ mapM m l

processSet :: [Either String Tag] -> M [Variables]
processSet x = wide (none <++> p) x
  where
    item b = do
      x <- processVariableSet b
      return [Variables $ M.fromList x]
    p = setVarTag ( \ a b -> setVar a b >> return [] )
      $ stepDown
      $ includeTag processSet 
      $ listingTag processSet
      $ programTag (parseRTPL >=> processSet)
      $ itemTag item
      $ unexpectedTag

processVariableSet :: Body -> M [(String,Variable)]
processVariableSet x = wide (none <++> p) x
  where
    p = setVarTag (\ a b -> return [(a,b)]) $ stepDown unexpectedTag

none :: a -> M [b]
none = const $ return []

unexpectedTag :: Tag -> M a
unexpectedTag (Tag pos name attrs body) = throwError $ "unexpected tag " ++ name

foreachTag :: ( Body -> Variables -> M [a] ) -> (Tag -> M [a]) -> Tag -> M [a]
foreachTag success cont (Tag pos "foreach" attrs body) = do
  let (var:c) = attrs
      count = case c of
        [] -> 9000
        x:_ -> read x
  v <- lookupVar $ head attrs
  set <- mcast v
  wide (success body) $ take count set
foreachTag _ cont t = cont t

variableTag :: (Variable -> M a) -> (Tag -> M a) -> Tag -> M a
variableTag success cont (Tag pos "variable" attrs body) = do
  v <- lookupVar $ head attrs
  success v
variableTag _ cont t = cont t


itemTag :: (Body -> M a) -> (Tag -> M a) -> Tag -> M a
itemTag success cont (Tag pos "item" attrs body) = success body
itemTag _ cont t = cont t

includeTag :: (Body -> M a) -> (Tag -> M a) -> Tag -> M a
includeTag success cont (Tag pos "include" attrs body) = do
  b <- processTemplate body
  t <- readRTPLE utf8 b
  recordDepend b
  let sv = Right $ Tag pos "setVar" ["path"] [Left $ takeDirectory b]
  success $ sv:t
includeTag _ cont t = cont t

setVarTag :: (String -> Variable -> M a) -> (Tag -> M a) -> Tag -> M a
setVarTag success cont (Tag pos "setVar" attrs body) = do
  let (varName:type') = attrs
  b <- case type' of
    ["set"] -> Variable `liftM` processSet body
    _ -> Variable `liftM` processTemplate body
  success varName b
setVarTag _ cont t = cont t

listingTag :: (Body -> M a) -> (Tag -> M a) -> Tag -> M a
listingTag success cont (Tag pos "listing" attrs body) = do
  x <- processTemplate body
  z <- wide g $ lines x
  success z
  where 
    g fp = do
      c <- liftIO $ doesFileExist $ "x:/" ++ fp ++ "/~phead.htm"
      parseRTPL $ if c 
        then printf "[item|[setVar:head|[include|%s/~phead.htm]]]" fp
        else error "FIXME: Process.hs 130"
   -- printf "[item|[setVar:head|[phpInclude|%s]]]" fp
listingTag _ cont t = cont t


programTag :: (String -> M a) -> (Tag -> M a) -> Tag -> M a
programTag success cont (Tag pos "program" (name:args) body) = do
  x <- processTemplate body
  let cp = (proc name args) 
           { std_in = CreatePipe
           , std_out = CreatePipe }
  r <- liftIO $ do 
    (Just sin, Just sout, Nothing, h) <- createProcess cp
    hPutStr sin x
    hClose sin
    hGetContents sout
  success r
programTag _ cont t = cont t

imagesTag :: (String -> M a) -> (Tag -> M a) -> Tag -> M a
imagesTag success cont t@(Tag pos "images" attrs body) = do
  b <- processTemplate body
  let Right r = P.parse parser "" $ concat $ words b
  success $ concatMap (join $ printf "<a href=\"%d.jpg\"><img src=\"%ds.jpg\"></a>") r
  where
    parser = do
      d <- di
      e <|> liftM (d:) comma <|> range d
    comma = P.char ',' >> parser
    range d = do
      P.string ".."
      d2 <- di
      liftM ([d..d2] ++) $ e <|> comma
    e = P.eof >> return []
    di :: P.Parsec String () Int
    di = liftM read $ P.many1 P.digit 
imagesTag _ cont t = cont t

stepDown :: (Tag -> M a) -> Tag -> M a
stepDown proc t@(Tag pos' name attrs body) = do
  stack %: \ l@(StackElem{..}:xs) -> StackElem{tag__ = t,..}:l
  x <- proc t
  stack %: tail
  return x

{-
runFile :: FilePath -> M String -> IO ()      
runFile fp m = do
  c <- isNeedRebuild fp
  case c of 
    False -> printf "skipping %s\n" fp
    True -> do
      printf "processing %s ... " fp
      hFlush stdout
      r <- runM $ do
        a <- m 
        deps <- A.get depends
        liftIO $ do
          writeFileE utf8 fp a
          updateRebuildInfo fp $ S.toList deps
      error . show <++> return $ r
      putStrLn "done"-}