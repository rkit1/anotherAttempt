module Path where
import Path.Destination
import Path.Source

-- LEGACY
import Text.Parsec hiding (State)
import Library

class Path a where
  isAbsolute :: a -> Bool
  rootPath :: a
  isRoot :: a -> Bool
  (</>) :: a -> a -> a
  goUp :: a -> a

instance Path DestinationPath where
  isAbsolute (DestinationPath ("":p)) = True
  isAbsolute _ = False
  rootPath = DestinationPath ["",""]
  isRoot (DestinationPath ["",""]) = True
  isRoot _ = False
  (DestinationPath a) </> (DestinationPath b) = (DestinationPath $ a ++ b)
  goUp (DestinationPath []) = (DestinationPath [])
  goUp (DestinationPath a) = (DestinationPath $ init a)


--instance Path SourcePath where


-- LEGACY
fixPath :: String -> String
fixPath str | Right r <- t = r
            | Left err <- t = error $ printf "fixPath: trying to process %s. Error is %s" (show err) str
  where 
    t = parse p "" str 
    p = msum
      [ try (oneOf "xX" >> char ':') >> win
      , unix ]
    win = many1 (slash `mplus` anyChar)
    slash = char '\\' >> return '/'
    unix = many1 anyChar
