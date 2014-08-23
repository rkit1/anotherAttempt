{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module PlainTemplate.Process where

import PlainTemplate.Variable
import PlainTemplate.Parser
import PlainTemplate.Monad
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
import SiteGen.IO
import SiteGen.Deps
import Data.String

callRTPL :: (PTLMonad si di m) => si -> m String
callRTPL path = do
  readRTPLE utf8 path >>= processTemplate


readRTPL :: (PTLMonad si di m) => si -> m Body
readRTPL path = do
  f <- readString path
  throwError . show <++> return 
    $ runP body () (show path) f

readRTPLE :: (PTLMonad si di m)
             => TextEncoding -> si -> m Body
readRTPLE enc path = do
  f <- readString $ path
  throwError . show <++> return 
    $ runP body () (show path) f

parseRTPL :: (MonadError String m) => String -> m Body
parseRTPL str = throwError . show <++> return  $ runP body () "no file" str

processTemplate :: PTLMonad si di m => Body -> m String
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

processSet :: PTLMonad si di m => [Either String Tag] -> m [Dictionary]
processSet x = wide (none <++> p) x
  where
    item b = do
      x <- processVariableSet b
      return [Dictionary $ M.fromList x]
    p = setVarTag ( \ a b -> setVar a b >> return [] )
      $ stepDown
      $ includeTag processSet 
      $ listingTag processSet
      $ programTag (parseRTPL >=> processSet)
      $ itemTag item
      $ unexpectedTag

processVariableSet :: PTLMonad si di m => Body -> m [(String,Variable)]
processVariableSet x = wide (none <++> p) x
  where
    p = setVarTag (\ a b -> return [(a,b)]) $ stepDown unexpectedTag

none :: PTLMonad si di m => a -> m [b]
none = const $ return []

unexpectedTag :: PTLMonad si di m => Tag -> m a
unexpectedTag (Tag pos name attrs body) = throwError $ "unexpected tag " ++ name

foreachTag :: PTLMonad si di m
              => (Body -> Dictionary -> m [a]) -> (Tag -> m [a]) -> Tag -> m [a]
foreachTag success cont (Tag pos "foreach" attrs body) = do
  let (var:c) = attrs
      count = case c of
        [] -> 9000
        x:_ -> read x
  v <- lookupVar $ head attrs
  set <- mcast v
  wide (success body) $ take count set
foreachTag _ cont t = cont t

variableTag :: PTLMonad si di m => (Variable -> m a) -> (Tag -> m a) -> Tag -> m a
variableTag success cont (Tag pos "variable" attrs body) = do
  v <- lookupVar $ head attrs
  success v
variableTag _ cont t = cont t


itemTag :: PTLMonad si di m => (Body -> m a) -> (Tag -> m a) -> Tag -> m a
itemTag success cont (Tag pos "item" attrs body) = success body
itemTag _ cont t = cont t

includeTag :: PTLMonad si di m => (Body -> m a) -> (Tag -> m a) -> Tag -> m a
includeTag success cont (Tag pos "include" attrs body) = do
  b <- processTemplate body
  let b' = fromString b
  t <- readRTPLE utf8 b'
  let sv = Right $ Tag pos "setVar" ["path"] [Left $ takeDirectory b]
  success $ sv:t
includeTag _ cont t = cont t

setVarTag :: PTLMonad si di m
             => (String -> Variable -> m a) -> (Tag -> m a) -> Tag -> m a
setVarTag success cont (Tag pos "setVar" attrs body) = do
  let (varName:type') = attrs
  b <- case type' of
    ["set"] -> Variable `liftM` processSet body
    _ -> Variable `liftM` processTemplate body
  success varName b
setVarTag _ cont t = cont t


listingTag :: PTLMonad si di m => (Body -> m a) -> (Tag -> m a) -> Tag -> m a
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

programTag :: PTLMonad si di m => (String -> m a) -> (Tag -> m a) -> Tag -> m a
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


imagesTag :: PTLMonad si di m => (String -> m a) -> (Tag -> m a) -> Tag -> m a
imagesTag success cont t@(Tag pos "images" attrs body) = do
  b <- processTemplate body
  let Right r = P.parse parser "" $ concat $ words b
  success $ concatMap
    (join $ printf "<a href=\"%d.jpg\"><img src=\"%ds.jpg\"></a>") r
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

stepDown :: PTLMonad si di m => (Tag -> m a) -> Tag -> m a
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
