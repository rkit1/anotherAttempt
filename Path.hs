module Path where
import Path.Destination
import Path.Source


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