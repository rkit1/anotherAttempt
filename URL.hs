{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, QuasiQuotes, FlexibleContexts, RecordWildCards, CPP #-}
module URL where
import Data.Maybe
import Text.Peggy
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.Error
import Data.String
import Text.Printf

{-
data URL = URL
  { host :: Maybe HostPart
  , resource :: ResourcePart }

data HostPart
  { scheme :: T.Text
  , auth :: Maybe T.Text
  , host :: T.Text
  , port :: Maybe Int }

data ResourcePart
  { path :: T.Text
  , query :: T.Text
  , fragment :: T.Text }
-}
data URL = URL
  { scheme :: String
  , auth :: String
  , host :: String
  , port :: Int
  , path :: String
  , query :: String
  , fragment :: String }
--  deriving Show

instance Show URL where
  show u = printf "URL <%s>" $ showURL u


-- FIXME encoding
showURL :: URL -> String
showURL URL{..} = outURL
  where 
    f "" x = ""
    f _ x = x
    auth' = f auth (auth ++ "@")
    port' | port == 0 = ""
          | otherwise = ':':show port
    query' = f query ('?':query)
    fragment' = f fragment ('#':fragment)
    outURL | host == "" = printf "%s%s%s" path query' fragment'
           | otherwise = printf "%s://%s%s%s%s%s%s" scheme auth' host port' path query' fragment'

urlRelativeTo :: URL -> URL -> URL
urlRelativeTo rel@URL{..} abs@URL{path = oldPath} 
  | scheme == "" =
    abs { path = path `pathRelativeTo` oldPath
        , query = query
        , fragment = fragment }
  | otherwise = rel

pathRelativeTo newPath@('/':_) _ = newPath
pathRelativeTo newPath oldPath = trimEnding ++ newPath
  where
    trimEnding = take (findLast 0 0 oldPath) oldPath
    findLast pos last ('/':xs) = findLast (pos+1) (pos+1) xs
    findLast pos last (x:xs) = findLast (pos+1) last xs
    findLast pos last [] = last

parseURLText :: MonadError String m => T.Text -> m URL
parseURLText t = parseURL $ T.unpack t

parseURL :: MonadError String m => String -> m URL
parseURL s = case parseString uRLP (printf "parseURL %s" s) s of
  Left err -> throwError $ show err
  Right res -> return res


instance IsString URL where
  fromString s = case parseString uRLP (printf "fromString %s" s) s of
    Left err -> error $ show err
    Right res -> res

----
-- Parsing
----

fm x Nothing = x
fm x (Just a) = a

[peggy|

schemeP :: String
  = [a-zA-Z] [a-zA-Z0-9+-.]* { $1 : $2 }

authP :: String
  = [^@]+

hostP :: String
  = [^:/?]+

portP :: Int
  = [0-9]+ { read $1 }

pathP :: String
  = [^?#]+

queryP :: String
  = [^#]*

fragmentP :: String
  = .*

uRLP :: URL
  = absoluteURLP 
  / relativeURLP 


relativeURLP :: URL
  = pathP ('?' queryP)? ('#' fragmentP)? { URL "" "" "" 0 $1 (fm "" $2) (fm "" $3) } 
                     
absoluteURLP :: URL
  = schemeP '://' (authP '@')? hostP (':' portP)? pathP ('?' queryP)? ('#' fragmentP)?
    { URL $1 (fm "" $2) $3 (fm 0 $4) $5 (fm "" $6) (fm "" $7) }

|] 


----
-- tests
----

#ifdef test

urlRelativeTOTests =
  [ fromRight (parseURLTests !! 1) `urlRelativeTo` fromRight (parseURLTests !! 0)
  , fromRight (parseURLTests !! 2) `urlRelativeTo` fromRight (parseURLTests !! 0)
  , fromRight (parseURLTests !! 0) `urlRelativeTo` fromRight (parseURLTests !! 2)
  , fromRight (parseURLTests !! 1) `urlRelativeTo` fromRight (parseURLTests !! 2)
  , fromRight (parseURLTests !! 2) `urlRelativeTo` fromRight (parseURLTests !! 2) ]
  where
    fromRight (Right a) = a


parseURLTests :: [Either String URL]
parseURLTests = map parseURL
  [ "http://www.facebook.com/groups/assembly.reception/"
  , "/groups/assembly.reception/#dsa"
  , "assembly.reception/?asd" ]

#endif