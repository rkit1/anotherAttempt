{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, RecordWildCards #-}
module ClubviRu.URIParser where
import Text.Peggy
import ClubviRu.Config.Site
import Data.List


filterLinks :: SiteConfig m => [String] -> m [String]
filterLinks links = do
  dmns <- myDomains
  let
    f (Right ParseResult{..}) | Just _ <- find (== host) ("":dmns) = [path]
    f _ = []
    parse str = parseString uRIP str str
  return $ links >>= f . parse
    
  


data ParseResult = ParseResult
  { host :: String
  , path :: String }
  deriving Show

[peggy|

schemeP :: ()
  = 'http://'
  / '//'

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

uRIP :: ParseResult
  = absoluteURIP 
  / relativeURIP 


relativeURIP :: ParseResult
  = pathP ('?' queryP)? ('#' fragmentP)? { ParseResult "" $1 } 
                     
absoluteURIP :: ParseResult
  = schemeP (authP '@')? hostP (':' portP)? pathP ('?' queryP)? ('#' fragmentP)?
    { ParseResult $3 $5 }

|] 


