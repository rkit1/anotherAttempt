{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, RecordWildCards #-}
module ClubviRu.URIParser where
import Text.Peggy
import ClubviRu.Config.Site
import Data.List


filterLinks :: SiteConfig m => [String] -> m [String]
filterLinks links = do
  dmns <- myDomains
  let
    f (Right HTTP{..}) | Just _ <- find (== host) (dmns) = ['/':path]
                       | host == "" = [path]
    f _ = []
    parse str = parseString uriP str str
  return $ links >>= f . parse

  


data ParseResult
  = HTTP
    { host :: String
    , path :: String }
  | Unknown
    { scheme :: String
    , content :: String }
  deriving Show

setHost x@HTTP{..} h = x {host = h}

[peggy|

httpSchemeP :: ()
  = 'http:' / 'https:'

anySchemeP :: String
  = [a-zA-Z]+ ':'

uriP :: ParseResult
  = httpSchemeP httpURIp { $2 }
  / anySchemeP .+ { Unknown $1 $2 }
  / httpURIp

httpURIp :: ParseResult
  = absoluteHttpURIp
  / relativeHttpURIp

relativeHttpURIp :: ParseResult
  = pathP ('?' queryP)? ('#' fragmentP)? { HTTP "" $1 } 

absoluteHttpURIp :: ParseResult
  = '//' (authP '@')? hostP (':' portP)? relativeHttpURIp { setHost $4 $2 } 


authP :: String
  = [^@]+

hostP :: String
  = [^:/?]+

portP :: Int
  = [0-9]+ { read $1 }

pathP :: String
  = [^?#]*

queryP :: String
  = [^#]*

fragmentP :: String
  = .*

|] 


