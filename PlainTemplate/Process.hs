{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module PlainTemplate.Process where

import PlainTemplate.Parser
import PlainTemplate.Monad
import Library
import Library.System
import qualified Data.Map as M
import qualified Data.Set as S
import System.Process
import System.FilePath
import qualified Text.Parsec as P
import SiteGen.Main
import Data.String
import Control.Eff

callRTPL path = do
  readRTPLE utf8 path >>= processTemplate


readRTPL path = do
  f <- readString path
  throwError . show <++> return 
    $ runP body () (show path) f

readRTPLE :: HasPTL si di t r => TextEncoding -> si -> Eff r Body
readRTPLE enc path = do
  f <- readString $ path
  throwError . show <++> return 
    $ runP body () (show path) f

parseRTPL :: HasPTL si di t r => String -> Eff r Body
parseRTPL str = throwError . show <++> return  $ runP body () "no file" str

processTemplate :: HasPTL si di t r => Body -> Eff r String
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

processSet :: HasPTL si di t r => [Either String Tag] -> Eff r [Dictionary]
processSet x = wide (none <++> p) x
  where
    item b = do
      x <- processVariableSet b
      return [Dictionary $ M.fromList x]
    p = setVarTag ( \ a b -> setVar a b >> return [] )
      $ stepDown
      $ includeTag processSet 
      $ programTag (parseRTPL >=> processSet)
      $ itemTag item
      $ unexpectedTag


processVariableSet :: HasPTL si di t r => Body -> Eff r [(String,Variable)]
processVariableSet x = wide (none <++> p) x
  where
    p = setVarTag (\ a b -> return [(a,b)]) $ stepDown unexpectedTag

none :: HasPTL si di t r => a -> Eff r [b]
none = const $ return []

unexpectedTag :: HasPTL si di t r => Tag -> Eff r a
unexpectedTag (Tag pos name attrs body) = throwError $ "unexpected tag " ++ name


foreachTag :: HasPTL si di t r
  => (Body -> Dictionary -> Eff r [a]) -> (Tag -> Eff r [a]) -> Tag -> Eff r [a]
foreachTag success cont (Tag pos "foreach" attrs body) = do
  let (var:c) = attrs
      count = case c of
        [] -> 9000
        x:_ -> read x
  v <- lookupVar $ head attrs
  set <- mcast v
  wide (success body) $ take count set
foreachTag _ cont t = cont t


variableTag :: HasPTL si di t r
  => (Variable -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
variableTag success cont (Tag pos "variable" attrs body) = do
  v <- lookupVar $ head attrs
  success v
variableTag _ cont t = cont t

itemTag :: HasPTL si di t r => (Body -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
itemTag success cont (Tag pos "item" attrs body) = success body
itemTag _ cont t = cont t

includeTag :: HasPTL si di t r => (Body -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
includeTag success cont (Tag pos "include" attrs body) = do
  b <- processTemplate body
  let b' = fromString b
  t <- readRTPLE utf8 b'
  let sv = Right $ Tag pos "setVar" ["path"] [Left $ takeDirectory b]
  success $ sv:t
includeTag _ cont t = cont t

setVarTag :: HasPTL si di t r
  => (String -> Variable -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
setVarTag success cont (Tag pos "setVar" attrs body) = do
  let (varName:type') = attrs
  b <- case type' of
    ["set"] -> Variable `liftM` processSet body
    _ -> Variable `liftM` processTemplate body
  success varName b
setVarTag _ cont t = cont t

programTag :: HasPTL si di t r => (String -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
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

imagesTag :: HasPTL si di t r => (String -> Eff r a) -> (Tag -> Eff r a) -> Tag -> Eff r a
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

stepDown :: HasPTL si di t r => (Tag -> Eff r a) -> Tag -> Eff r a
stepDown proc t@(Tag pos' name attrs body) = do
  sStack %: \ l@(StackElem{..}:xs) -> StackElem{seTag__ = t,..}:l
  x <- proc t
  sStack %: tail
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
