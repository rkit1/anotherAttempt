{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, QuasiQuotes, FlexibleContexts, RecordWildCards #-}
module URL where
import Data.Typeable
import Data.SafeCopy
import Control.Monad.Error
import Data.Maybe
import Text.Peggy
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.Error
import Data.String
import Text.Printf

-----
-- Инстансы isString
-- Тесты
-----


newtype LocalResourceURL = LocalResourceURL { getSrcFilePath :: FilePath } 

data RelativeHTTPURL = RelativeHTTPURL
  { path :: String
  , query :: String
  , fragment :: String }
  deriving Show
$(deriveSafeCopy 0 'base ''RelativeHTTPURL)
  
data HTTPURL = HTTPURL
  { host :: String
  , auth :: (String, String)
  , port :: Int
  , relativePart :: RelativeHTTPURL }
  deriving Show
$(deriveSafeCopy 0 'base ''HTTPURL)

newtype StoreURL = StoreURL String deriving (Ord, Eq, Typeable, Show)
$(deriveSafeCopy 0 'base ''StoreURL)
  
data UnknownURL = UnknownURL
  { scheme :: String
  , content :: String }

data URL
  = RelativeHTTPURL_ RelativeHTTPURL
  | HTTPURL_ HTTPURL
  | StoreURL_ StoreURL
  | UnknownURL_ UnknownURL

urlRelativeTo :: RelativeHTTPURL -> HTTPURL -> HTTPURL
urlRelativeTo rel@RelativeHTTPURL{..} abs@HTTPURL{..} = abs{relativePart = newRel}
  where
    RelativeHTTPURL {path = oldPath} = relativePart
    newRel = rel{path = path `pathRelativeTo` oldPath}

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
parseURL s = case parseString uRLP "" s of
  Left err -> throwError $ show err
  Right res -> return res

----
-- isString
---- 

instance IsString RelativeHTTPURL where
  fromString s = case parseString relativeURLP (printf "<fromString %s>" s) s of
    Left err -> error $ show err
    Right res -> res

instance IsString HTTPURL where
  fromString s = case parseString absoluteURLP (printf "<fromString %s>" s) s of
    Left err -> error $ show err
    Right res -> res

----
-- Parsing
----

uRLHelper scheme auth host port path query fragment =
  HTTPURL
    { host = host
    , port = fromMaybe 80 port
    , auth = fromMaybe ("","") auth
    , relativePart = relativeURLHelper path query fragment }

relativeURLHelper path query fragment = 
  RelativeHTTPURL
    { path = path
    , query = fromMaybe "" query
    , fragment = fromMaybe "" fragment }

[peggy|

schemeP :: String
  = [a-zA-Z] [a-zA-Z0-9+-.]* { $1 : $2 }

authP :: (String, String)
  = [^@:/?]+ (':' [^@]+)? { ($1, fromMaybe "" $2) }

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
  = absoluteURLP { HTTPURL_ $1 }
  / relativeURLP { RelativeHTTPURL_ $1 }
  / unknownURLP { UnknownURL_ $1 }

relativeURLP :: RelativeHTTPURL
  = pathP ('?' queryP)? ('#' fragmentP)? { relativeURLHelper $1 $2 $3 } 
                     
absoluteURLP :: HTTPURL
  = schemeP '://' (authP '@')? hostP (':' portP)? pathP ('?' queryP)? ('#' fragmentP)?
    { uRLHelper $1 $2 $3 $4 $5 $6 $7 }

unknownURLP :: UnknownURL
  = schemeP ':' .* { UnknownURL $1 $2 }

|] 


----
-- tests
----
{-
urlRelativeTOTests =
  [ fromRight (parseURLTests !! 1) `urlRelativeTo` fromLeft (parseURLTests !! 0)
  , fromRight (parseURLTests !! 2) `urlRelativeTo` fromLeft (parseURLTests !! 0) ]
  where
    fromLeft (Left a) = a
    fromRight (Right a) = a


parseURLTests = map parseURL
  [ "http://www.facebook.com/groups/assembly.reception/"
  , "/groups/assembly.reception/"
  , "assembly.reception/" ]

-}